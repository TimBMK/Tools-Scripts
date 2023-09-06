# Parallel Snapshot function

## A function to calculate network metrics for snapshot networks - page rank and communities, specifically
## Additionally supports calculating the PMI as a weight in the snapshots, making it suitable for e.g. document-token networks
## Remember that calculating the PMI effectively projects a bipartite network to monopoartite
## Supports multithreading via furrr. If multithreading is desired, remember to set the workers with future::plan(multisession, workers = ...) before executing the function (else it is single threaded)

snapshots <- function(data,      # a data frame
                      vertex_a,  # first vertex. if pmi is calculated as weight, this is used as the feature column
                      vertex_b,  # second vertex. if pmi is caclulated as weight, this is used as the item column
                      time,      # time indicator to slice into. correct time slices need to be provided in the data frame (e.g. day, week, month...), is used as-is
                      directed = FALSE, # should the graph passed to the community detection be directed? If TRUE, then vertex_a -> vertex_b. Always undirected if PMI weighted
                      pmi_weight = TRUE, # should the PMI be calculated for the slices and used as weights? if true make sure to specify vertex a and b correctly. Effectively projects a bipartite graph to monopartite
                      output = c("metrics", "networks"), # should metrics for each snapshot or the networks itself be the output? page_rank and community only work for metrics. If pmi_weight = T, this outputs the pmi-weighted network for each snapshot as a dataframe
                      page_rank = TRUE, # should the pagerank for nodes in each snapshot be calculated? Only if output = "metrics"
                      negative_pagerank_weights = TRUE, # Should negative weights be used for the pagerank calculation? If FALSE, negative weights are normalized to 0
                      degree = FALSE,
                      community = TRUE, # should communities be calculated? if yes, specify a community function (default is Leiden). Only if output = "metrics
                      community_function = igraph::cluster_leiden, # provide the community detection function here, as provided by the igraph package (other functions are untested!). Only if output = "metrics
                      seed = NULL, # fixes RNG issues in parallelization. NULL only supresses warnings!
                      ...) { # ... to pass on arguments to algorithm, e.g. objective_function or resolution_paramter for cluster_leiden
  
  require(dplyr)
  require(tidyr)
  require(igraph)
  require(data.table)
  require(furrr)
  require(widyr)
  
  output <- match.arg(output)
  
  
  timeframes <-  split(data.table::as.data.table(data), by = time)
  
  snapshot <-             # function to be mapped over timeframes
    function(timeframe,
             vertex_a,
             vertex_b,
             directed,
             pmi_weight,
             page_rank,
             community,
             community_function,
             ...) {
      
      if (pmi_weight == TRUE) {
        suppressWarnings({ # suppress warnings about deprecated matrix function in pmi calculation
          slice <-
            timeframe %>%
            dplyr::select({{ vertex_a }}, {{ vertex_b }}) %>%
            widyr::pairwise_pmi_(feature =  {{vertex_a}}, item = {{vertex_b}}, sort = F) %>% dplyr::rename(weight = pmi) %>% # calculate PMI as weight (use pairwise_pmi_() avoid problems with column specification)
            igraph::graph_from_data_frame(directed = F) # make igraph object for slice
        })
        
      } else {
        # unweighted if not pmi-weighted
        slice <-
          timeframe %>% dplyr::as_tibble() %>%
          dplyr::select({{ vertex_a }}, {{ vertex_b }}) %>%
          igraph::graph_from_data_frame(directed = directed) # make igraph object for slice
      }
      
      if (output == "metrics") {
        slice_dat <- dplyr::tibble(node = V(slice)$name,
                                  time = timeframe %>% 
                                    dplyr::distinct(!!as.name(time)) %>% 
                                    dplyr::pull()) # get time from the timeframe
        
        try({
          # try() to avoid failure on empty slices
          
          if (community == TRUE) {
            communities <-
              community_function(slice, ...) # calculate communities
            slice_dat <-
              slice_dat %>% dplyr::left_join(tibble(
                node = V(slice)$name,
                community = communities$membership
              ),
              by = "node")
          }
          
          if (page_rank == TRUE) {
            
            if (negative_pagerank_weights == FALSE) {
              slice_pagerank <- igraph::subgraph.edges(slice, which(E(slice)$weight > 0)) 
              page_rank <- igraph::page_rank(slice_pagerank)
            } else {
              page_rank <- igraph::page_rank(slice) # calculate page rank on negative weights
            }
            
            slice_dat <-
              slice_dat %>% dplyr::left_join(
                dplyr::tibble(
                  node = V(slice)$name,
                  page_rank = page_rank$vector
                ),
                by = "node") %>% 
              tidyr::replace_na(list(page_rank = 0))
            
          }
          
          if (degree == TRUE) {
            slice_degree <- igraph::subgraph.edges(slice, which(E(slice)$weight > 0)) # Normalize weight by removing edges with negative weights
            degree <- igraph::degree(slice_degree, mode = "total") # calculate degree
            slice_dat <- 
              slice_dat %>% dplyr::left_join(
                dplyr::tibble(
                  node = V(slice_degree)$name,
                  degree = degree
                ),
                by = "node") %>% 
              tidyr::replace_na(list(degree = 0))
          }
          
        })
        
      }
      
      if (output == "networks") {
        slice_dat <- igraph::as_data_frame(slice, what = "edges") %>%
          dplyr::mutate(time = timeframe %>% 
                          dplyr::distinct(!!as.name(time)) %>% 
                          dplyr::pull()) %>% 
          dplyr::distinct(from, to, .keep_all = TRUE) # remove duplicated edges (a to b, b to a)
        
      }
      
      return(slice_dat)
      
    }

  
  output <-                    # mapping the snapshot function over each timeframe
    furrr::future_map(
      timeframes,
      function(x, ...)
        snapshot(x,
                 vertex_a = vertex_a,
                 vertex_b = vertex_b,
                 directed = directed,
                 pmi_weight = pmi_weight,
                 page_rank = page_rank,
                 community = community,
                 community_function = community_function,
                 ...),
      .options = furrr_options(seed = seed), # to fix RNG issues in the parallelization
      ... # triple dot construct with anonymous function to pass ... properly
    ) %>% bind_rows()
  
  return(output)
  
}


