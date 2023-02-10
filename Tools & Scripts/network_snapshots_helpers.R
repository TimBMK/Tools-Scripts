# a collection of helper functions to process and compare temporal network models


require(tidyverse)
require(purrr)
require(DescTools)


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
    topics %>% group_by(temporal_community) %>% summarise(n = n()) %>%
      mutate(percentage = n / sum(n))  %>% arrange(desc(n)) %>% # check distribution
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

get_top_terms <- function(topics_full, n = 20, # n sets the number of terms returned per topic;
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


make_model_overview <- function(...,                # model objects go here
                                model_names = NULL) { # optional c(...) of model names (else ... names)){             
  if (is.null(model_names)) {
    models <- match.call(expand.dots = F)
    model_names <- as.character(models$...)
  }
  model_list = list(...)
  tibble(
    model = model_names,
    topics = model_list %>% map_dbl(~ .x %>% filter(!is.na(topic)) %>% distinct(topic) %>% nrow()),
    topicless_entities = model_list %>% map_dbl(~ .x %>% filter(is.na(topic)) %>% distinct(node) %>% nrow()),
    topic_entities = model_list %>% map_dbl(~ .x %>% filter(!is.na(topic)) %>% distinct(node) %>% nrow()),
    average_entities_per_topic = model_list %>% map_dbl(~ .x %>% filter(!is.na(topic)) %>% summarise(mean(total_entitites_in_topic)) %>% pull(1)),
    average_topic_occurence = model_list %>% map_dbl(~ .x %>% filter(!is.na(topic)) %>% summarise(mean(total_topic_occurrences_global)) %>% pull(1)),
    average_topics_per_term = model_list %>% map_dbl(~ .x %>% filter(!is.na(topic)) %>% group_by(node) %>% summarise(topics = n()) %>% ungroup() %>% summarise(mean = mean(topics)) %>% pull(1)),
    topic_entities_gini = model_list %>% map_dbl(~ .x %>% filter(!is.na(topic)) %>% distinct(topic, .keep_all = T) %>% pull(total_entitites_in_topic) %>% Gini()),
    topic_occurence_gini = model_list %>% map_dbl(~ .x %>% filter(!is.na(topic)) %>% distinct(topic, .keep_all = T) %>% pull(total_topic_occurrences_global) %>% Gini())
  )
}
