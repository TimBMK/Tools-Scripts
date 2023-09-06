# a collection of helper functions to process and compare temporal network models


require(tidyverse)
require(purrr)
require(DescTools)
require(tnet)
require(tidytext)
require(data.table)
require(furrr)
require(widyr)


clusters_to_topics <- function(snapshots, clusters, statistics = TRUE) {
  topics <- snapshots %>%
    # mutate(
    #   snapshot = recode(as.character(time),!!!setNames( # recode to numeric indicator
    #     1:length(unique(.$time)), # setNames provides the levels
    #     unique(.$time)
    #   )) - 1,     # snapshot indicator adjusted for python counting starting at 0
    #   community = as.numeric(community)
    # ) %>%
    left_join(tibble(time = snapshots %>% distinct(time) %>% pull(time) %>% as_date(), # this is more efficient than recoding
                     snapshot = 1:length(snapshots %>% distinct(time) %>% pull(time))-1), by = "time") %>% # snapshot indicator adjusted for python counting starting at 0
    mutate(community = as.numeric(community)) %>% 
    left_join(clusters, by = c("snapshot", "community")) %>% 
    ungroup()
  
  if (statistics == T) {
    topics %>% group_by(temporal_community) %>% summarise(nodes = n()) %>%
      mutate(percentage = nodes / sum(nodes))  %>% arrange(desc(nodes)) %>% # check distribution
      print()
  }
  return(topics)
}


full_topic_overview <- function(topic_clusters, combined_NE, time_column, 
                                topics_full = NULL) { # optional data of full topics with global page_rank. If provided, orders by global page rank. If not, average. If none are available, no page rank ordering within topics
  full_topics <- topic_clusters %>% 
    rename(topic = temporal_community, snapshot_topic = community) %>% # rename for clarity
    left_join( # global document count of tokens
      combined_NE %>% distinct(doc_id, lemma) %>% group_by(lemma) %>% summarise(global_document_count = n()),
      by = c("node" = "lemma")
    ) %>% 
    left_join( # snapshot document count of tokens
      combined_NE %>% distinct(doc_id, lemma, !!as.name(time_column)) %>% group_by(lemma, !!as.name(time_column)) %>% summarise(snapshot_document_count = n()),
      by = c("node" = "lemma", "time" = time_column)
    ) %>% 
    group_by(topic, time) %>% mutate( # snapshot topic occurrences
      total_topic_occurrences_snapshot = sum(snapshot_document_count)) %>% 
    group_by(topic) %>% mutate( # add overview statistics
      total_entitites_in_topic = n(),
      total_topic_occurrences_global = sum(global_document_count),
      average_topic_occurrences = mean(total_topic_occurrences_snapshot)
    ) %>% ungroup()
    
  if ("page_rank" %in% names(topic_clusters)) {
    full_topics <- full_topics %>% 
    group_by(node) %>% 
      mutate(
        average_page_rank =  mean(page_rank) # if pagerank for snapshots was calculated (NA if not)
      ) %>% 
      arrange(desc(total_topic_occurrences_global),
              topic,
              desc(average_page_rank))
  } else {
    full_topics <- full_topics %>%
      arrange(desc(total_topic_occurrences_global),
              topic)
  }
  
  if (!is.null(topics_full)) {
    full_topics <- full_topics %>% 
      left_join( # add global page_rank (if available)
        topics_full %>% distinct(lemma, page_rank) %>% rename(global_page_rank = page_rank),
        by = c("node" = "lemma")
      ) %>% 
      arrange(desc(total_topic_occurrences_global),
              topic,
              desc(global_page_rank))
  } 
  
  return(ungroup(full_topics))
  ### ! All overview statistics are valid for the topic, not the snapshot topic ! ###
}

get_top_terms_simple <- function(topics_full, n = 20, # n sets the number of terms returned per topic;
                          page_rank = c("average", "global")){ #  page_rank sets type of page rank metric used for picking the n top terms: average over snapshots or global calculation. Default is average
  
  page_rank <- match.arg(page_rank)
  
  if (page_rank == "average"){
    top_terms <- topics_full %>% filter(!is.na(topic)) %>%
      distinct(node, topic, average_page_rank, total_topic_occurrences_global) %>% group_by(topic) %>%
      slice_max(average_page_rank, n = n) %>% # top 20 terms per topic
      arrange(desc(total_topic_occurrences_global),
              topic,
              desc(average_page_rank)) %>%
      ungroup()
  }
  
  if (page_rank == "global"){
    top_terms <- topics_full %>% filter(!is.na(topic)) %>%
      distinct(node, topic, global_page_rank, total_topic_occurrences_global) %>% group_by(topic) %>%
      slice_max(global_page_rank, n = n) %>% # top 20 terms per topic
      arrange(desc(total_topic_occurrences_global),
              topic,
              desc(global_page_rank)) %>%
      ungroup()
  }
  
  return(top_terms)
}


get_top_documents_simple <-
  function(topics_full,    # topics_full made by full_topic_overview(). a doc_id variable and a topic variable is expected
           full_documents, # dataframe with the full documents
           join_variable = "doc_id", # this accepts all dplyr join "by = " arguments (incl. join_by())
           n = 5,          # number of docs returned per topics
           ties = F) {     # slice_max()'s with_ties operator
    
    top_documents <- topics_full %>%
      group_by(doc_id) %>% mutate(relative_topic_occurence = doc_topic_occurrences /
                                    sum(doc_topic_occurrences)) %>%
      group_by(topic) %>% slice_max(relative_topic_occurence,
                                    n = n,
                                    with_ties = ties) %>%
      left_join(full_documents,
                by = join_variable) %>% 
      ungroup()
    
  }



get_topic_documents <- 
  function(topics_counts,
           topics_full, # usually the output of full_topic_overview. must include the topic and a "terms" column named identical to the one in the tokens object
           tokens,
           id = "doc_id", # ID of the documents. must be identical in full_documents, tokens and topic_count
           terms = "lemma", # name of the node column
           full_documents, # data containing the full texts of the documents and all desired output columns (e.g. header). document ID must be named as in the id variable
           tf_idf_weight = TRUE, # should the projection of the term-document-network weighted by the tf-idf of the terms to weigh documents more highly where the topic terms are central?
           n = 5, # number of top documents returned
           method = "Newman") { # method for handling the weight calculation of the projection. See help(projecting_tm). "Newman" may not produce documents for topics with few documents (throws error in tapply)
    
    
    topic_counts_list <-  split(data.table::as.data.table(topics_counts), by = "topic")
    
    full_documents <- full_documents %>% dplyr::mutate({{id}} := as.character(!!as.name(id))) # make sure document IDs are characters
    
    pagerank_documents <- function(topic_count, # this is the actual funtion to be wrapped in future_map below (for efficiency)
                                   topics_full,
                                   tokens,
                                   id,
                                   terms,
                                   full_documents,
                                   tf_idf_weight,
                                   n,
                                   method){
      
      topic_docs <- topic_count %>% dplyr::filter(doc_topic_occurrences > 0) %>% dplyr::mutate({{id}} := as.character(!!as.name(id)))
      
      topic_terms <- topics_full %>% dplyr::filter(topic == (dplyr::distinct(topic_count, topic) %>% dplyr::pull()))  %>% dplyr::select({{terms}}, topic)
          
      topic_data <- tokens %>% 
        dplyr::filter(!!as.name(id) %in% (topic_docs %>% dplyr::pull({{id}}))) %>% 
        dplyr::count(!!as.name(id), !!as.name(terms), sort = T) %>% 
        dplyr::group_by(!!as.name(terms)) %>% dplyr::mutate(term_id = cur_group_id()) %>%  # integer IDs for lemmas and docs, as required by projecting_tm
        dplyr::group_by(!!as.name(id)) %>% dplyr::mutate(temp_doc_id = cur_group_id()) %>% # these are generated for docs in case they are not coercible to integer (e.g. char IDs)
        dplyr::ungroup() 
      
      
          if ((topic_docs %>% dplyr::distinct(!!as.name(id)) %>% nrow() > 1) &
              (topic_terms %>% dplyr::distinct(!!as.name(terms)) %>% nrow() > 1)) { # make sure we have more than 1 document & more than 1 term
            

            if (tf_idf_weight == T) {
              
              topic_data <- topic_data %>% 
                tidytext::bind_tf_idf(term = !!as.name(terms), document = !!as.name(id), n = n) %>% 
                dplyr::filter(!!as.name(terms) %in% (topic_terms %>% dplyr::pull({{terms}})))
              
              topic_graph_data <- 
                tnet::projecting_tm(topic_data %>% 
                                      dplyr::select(temp_doc_id, term_id, # order is important here: the first node is the one being projected, i.e. the resulting network
                                                    tf_idf), # the tf_idf serves as weight
                                    method = method) 
              
            } else {
              
              topic_data <- topic_data %>% 
                dplyr::filter(!!as.name(terms) %in% (topic_terms %>% dplyr::pull({{terms}})))
              
              topic_graph_data <- 
                tnet::projecting_tm(topic_data %>% 
                                      dplyr::select(temp_doc_id, term_id), # order is important here: the first node is the one being projected, i.e. the resulting network
                                    method = method) 
              
            }
            
            topic_graph <- igraph::graph_from_data_frame(topic_graph_data %>% dplyr::rename(weight = w), directed = F)
            
            igraph::V(topic_graph)$page_rank <- igraph::page_rank(topic_graph)$vector
            
            top_documents <- dplyr::tibble(temp_doc_id = as.integer(V(topic_graph)$name), 
                                           page_rank = unlist(V(topic_graph)$page_rank)) %>% 
              dplyr::slice_max(page_rank, n = n, with_ties = F) %>% 
              dplyr::mutate(topic = unique(topic_docs$topic)) %>% 
              dplyr::left_join(topic_data %>% dplyr::distinct(!!as.name(id), temp_doc_id), by = "temp_doc_id", multiple = "all") %>% 
              dplyr::left_join(full_documents, by = dplyr::join_by({{id}})) %>% 
              dplyr::select(!temp_doc_id)
            
            
            
          } 
      
      if (topic_docs %>% dplyr::distinct(!!as.name(id)) %>% nrow() == 1) { # if we only have one document, return this with page_rank 1
            
        # cat(paste0("\n Only one Document for Topic ", distinct(topic_count, topic) %>% pull(), ". Page Rank is set to 1 for this Document.\n"))
        
        top_documents <- topic_docs %>% dplyr::distinct(!!as.name(id)) %>% 
          dplyr::mutate(page_rank = 1,
                        topic = unique(topic_docs$topic)) %>% 
          dplyr::left_join(full_documents, by = dplyr::join_by({{id}})) 
      }
      
      if (topic_terms %>% dplyr::distinct(!!as.name(terms)) %>% nrow() == 1) { # if we only have one term, return documents with the highest tf-idfs
        
        cat(paste0("\n Only one Term in Topic ", dplyr::distinct(topic_count, topic) %>% dplyr::pull(), ". Returning Document Term Frequency instead of Page Rank.\n"))

        # as we only have one term, and all documents in the topic sample contain this term, the IDF, and therefore the TF-IDF would always be zero. 
        #   Therefore, a TF-IDF-weighted Page Rank would return no documents (the edge weight being 0 in all cases), and an unweighted Page Rank would return the same rank for all documents in which the term occurs
        
        topic_data <- topic_data %>% 
          tidytext::bind_tf_idf(term = !!as.name(terms), document = !!as.name(id), n = n) %>% 
          dplyr::filter(!!as.name(terms) %in% (topic_terms %>% dplyr::pull({{terms}})))
        
        top_documents <- topic_data %>% 
          dplyr::slice_max(tf, n = n, with_ties = F) %>% 
          dplyr::mutate(topic = unique(topic_docs$topic)) %>%
          dplyr::left_join(full_documents, by = dplyr::join_by({{id}})) %>% 
          dplyr::select(tf, topic, names(full_documents))
        
      }
          
          return(top_documents)

    }
    
    possibly_pagerank_documents <- purrr::possibly(pagerank_documents, otherwise = NULL) # this is a better solution than try(), as it returns NULL rather than an error message
    
    output <- 
      topic_counts_list %>% 
      furrr::future_map(
        possibly_pagerank_documents,
        topics_full = topics_full, 
        tokens = tokens,
        id = id,
        terms = terms,
        full_documents = full_documents, 
        tf_idf_weight = tf_idf_weight,
        n = n, # number of top documents returned
        method = method) %>% 
      purrr::compact() %>% # this removes the empty entries (i.e. topics without any retrieved documents / errors) defined through possibly() above
      dplyr::bind_rows()
    
    if (topics_counts %>% dplyr::distinct(topic) %>% 
        dplyr::filter(!(topic %in% (output %>% dplyr::distinct(topic) %>% dplyr::pull()))) %>% nrow() > 0) {
      
      cat("\n No Documents returned for Topic(s)", paste(topics_counts %>% dplyr::distinct(topic) %>% 
                                                           dplyr::filter(!(topic %in% (output %>% 
                                                                                         dplyr::distinct(topic) %>% 
                                                                                         dplyr::pull()))) %>% 
                                                           dplyr::pull(), collapse = ", "), "\n\n")
    }

    return(output)
    
  }

# top_docs <- get_topic_documents(topics_counts = topics_counts,
#                                 topics_full = topics_full,
#                                 tokens = news_tokens,
#                                 full_documents = news_data %>% rename(doc_id = ID, article_topic = topic) %>% select(doc_id, header, text, abstract))



get_topic_terms <- 
  function(topics_counts,
           topics_full, # usually the output of full_topic_overview. must include the topic and a "terms" column named identical to the one in the tokens object
           tokens, # a full tokens dataframe including information on doc_id and terms
           id = "doc_id", # ID of the documents. must be identical in full_documents, tokens and topic_count
           terms = "lemma", # name of the node column
           n = 5, # number of top documents returned
           method = "PMI") { # method for handling the weight calculation of the projection. Either PMI-weighted or a method from projecting_tm(), e.g. "Newman" (See help(projecting_tm)).
    
    id <- match.arg(id)
    terms <- match.arg(terms)
    method <- match.arg(method)
    
    topic_counts_list <-  split(data.table::as.data.table(topics_counts), by = "topic")
    
    
    pagerank_terms<- function(topic_count, # this is the actual funtion to be wrapped in future_map below (for efficiency)
                              topics_full,
                              tokens,
                              id,
                              terms,
                              n,
                              method){
      
      topic_docs <- topic_count %>% dplyr::filter(doc_topic_occurrences > 0)
      
      topic_terms <- topics_full %>% 
        dplyr::filter(topic == (dplyr::distinct(topic_count, topic) %>% 
                                  dplyr::pull())) %>% 
        dplyr::select({{terms}}, topic)
      
      if ((topic_docs %>% dplyr::distinct(!!as.name(id)) %>% nrow() > 1) & 
          (topic_terms %>% dplyr::distinct(!!as.name(terms)) %>% nrow > 1)) { # make sure we have more than 1 term & more than 1 document
        
        
        if (method == "PMI") {
          
          topic_data <- tokens %>% 
            dplyr::filter(!!as.name(id) %in% (topic_docs %>% dplyr::pull({{id}})) &        # only documents associated with the topic
                     !!as.name(terms) %in% (topic_terms %>% dplyr::pull({{terms}}))) # only terms from the topics
          
          suppressWarnings({ # suppress warnings about deprecated matrix function in pmi calculation
            topic_graph <-
              topic_data %>%
              dplyr::select({{id}}, {{terms}}) %>%
              widyr::pairwise_pmi_(feature =  {{id}}, item = {{terms}}, sort = F) %>% dplyr::rename(weight = pmi) %>% # calculate PMI as weight (use pairwise_pmi_() avoid problems with column specification)
              igraph::graph_from_data_frame(directed = F) # make igraph object for slice
          })
          
        } else {
          
          topic_data <- tokens %>% 
            dplyr::filter(!!as.name(id) %in% (topic_docs %>% dplyr::pull({{id}})) &
                     !!as.name(terms) %in% (topic_terms %>% dplyr::pull({{terms}}))) %>% 
            dplyr::count(!!as.name(id), !!as.name(terms), sort = T) %>% 
            dplyr::group_by(!!as.name(terms)) %>% dplyr::mutate(term_id = cur_group_id()) %>%  # integer IDs for lemmas and docs, as required by projecting_tm
            dplyr::group_by(!!as.name(id)) %>% dplyr::mutate(temp_doc_id = cur_group_id()) %>% # these are generated for docs in case they are not coercible to integer (e.g. char IDs)
            dplyr::ungroup() 
          
          # project the network with another method (e.g. "Newman")
          topic_graph <- 
            tnet::projecting_tm(topic_data %>% 
                            dplyr::select(term_id, temp_doc_id, # order is important here: the first node is the one being projected, i.e. the resulting network
                                   n), # we weigh the network by the raw count of tokens in a document
                          method = method) %>% 
            dplyr::rename(weight = w) %>% 
            igraph::graph_from_data_frame(directed = F)
          
          igraph::V(topic_graph)$name <-        # replace term ID with actual term
            tibble::tibble(term_id = igraph::V(topic_graph)$name %>% as.integer()) %>% 
            dplyr::left_join(topic_data %>% dplyr::distinct(!!as.name(terms), term_id), by = "term_id", multiple = "all") %>% 
            dplyr::pull({{terms}})
          
        }
        
        igraph::V(topic_graph)$page_rank <- igraph::page_rank(topic_graph)$vector
        
        top_terms <- tibble::tibble({{terms}} := igraph::V(topic_graph)$name, 
                            page_rank = unlist(igraph::V(topic_graph)$page_rank)) %>% 
          dplyr::slice_max(page_rank, n = n, with_ties = F) %>% 
          dplyr::mutate(topic = unique(topic_docs$topic)) 
        
        
        
      } else { # if we only have one document or one term, return terms with page_rank 1
        
        top_terms <- topic_terms %>% 
          dplyr::distinct(!!as.name(terms)) %>% 
          dplyr::mutate(page_rank = 1,
                        topic = unique(topic_docs$topic)) 
      }
      
      return(top_terms)
      
    }
    
    possibly_pagerank_terms <- purrr::possibly(pagerank_terms, otherwise = NULL) # this is a better solution than try(), as it returns NULL rather than an error message
    
    output <- 
      topic_counts_list %>% 
      furrr::future_map(
        possibly_pagerank_terms,
        topics_full = topics_full, 
        tokens = tokens,
        id = id,
        terms = terms,
        n = n, # number of top documents returned
        method = method) %>% 
      purrr::compact() %>% # this removes the empty entries (i.e. topics without any retrieved documents / errors) defined through possibly() above
      dplyr::bind_rows()
    
    if (topics_counts %>% dplyr::distinct(topic) %>% 
        dplyr::filter(!(topic %in% (output %>% dplyr::distinct(topic) %>% 
                                    dplyr::pull()))) %>% nrow() > 0) {
      
      cat("\n No terms returned for topics", paste(topics_counts %>% 
                                                     dplyr::distinct(topic) %>% 
                                                     dplyr::filter(!(topic %in% (output %>% 
                                                                                   dplyr::distinct(topic) %>% 
                                                                                   dplyr::pull()))) %>% 
                                                     dplyr::pull(), collapse = ", "), "\n\n")
      
    }

    
    return(output)
    
  }




make_model_overview <- function(...,                # model objects go here
                                model_names = NULL) { # optional c(...) of model names (else ... names)){             
  if (is.null(model_names)) {
    models <- match.call(expand.dots = F)
    model_names <- as.character(models$...)
  }
  model_list = list(...)
  tibble::tibble(
    model = model_names,
    topics = model_list %>% purrr::map_dbl(~ .x %>% dplyr::filter(!is.na(topic)) %>% 
                                             dplyr::distinct(topic) %>% nrow()),
    topicless_entities = model_list %>% purrr::map_dbl(~ .x %>% dplyr::filter(is.na(topic)) %>% 
                                                         dplyr::distinct(node) %>% nrow()),
    topic_entities = model_list %>% purrr::map_dbl(~ .x %>% dplyr::filter(!is.na(topic)) %>% 
                                                     dplyr::distinct(node) %>% nrow()),
    average_entities_per_topic = model_list %>% purrr::map_dbl(~ .x %>% dplyr::filter(!is.na(topic)) %>% 
                                                                 dplyr::summarise(mean(total_entitites_in_topic)) %>% 
                                                                 dplyr::pull(1)),
    average_topic_occurence = model_list %>% purrr::map_dbl(~ .x %>% filter(!is.na(topic)) %>% 
                                                              dplyr::summarise(mean(total_topic_occurrences_global)) %>% 
                                                              dplyr::pull(1)),
    average_topics_per_term = model_list %>% purrr::map_dbl(~ .x %>% dplyr::filter(!is.na(topic)) %>% 
                                                              dplyr::group_by(node) %>% dplyr::summarise(topics = n()) %>%
                                                              dplyr::ungroup() %>% dplyr::summarise(mean = mean(topics)) %>% 
                                                              dplyr::pull(1)),
    topic_entities_gini = model_list %>% purrr::map_dbl(~ .x %>% dplyr::filter(!is.na(topic)) %>% 
                                                          dplyr::distinct(topic, .keep_all = T) %>% 
                                                          dplyr::pull(total_entitites_in_topic) %>% 
                                                          DescTools::Gini()),
    topic_occurence_gini = model_list %>% purrr::map_dbl(~ .x %>% dplyr::filter(!is.na(topic)) %>% 
                                                           dplyr::distinct(topic, .keep_all = T) %>% 
                                                           dplyr::pull(total_topic_occurrences_global) %>% 
                                                           DescTools::Gini())
  )
}
