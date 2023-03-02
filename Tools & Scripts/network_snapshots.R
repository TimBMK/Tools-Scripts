# Parallel Snapshot function

## A function to calculate network metrics for snapshot networks - page rank and communities, specifically
## Additionally supports calculating the PMI as a weight in the snapshots, making it suitable for e.g. document-token networks
## Remember that calculating the PMI effectively projects a bipartite network to monopoartite
## Supports multithreading via furrr. If multithreading is desired, remember to set the workers with future::plan(multisession, workers = ...) before executing the function (else it is single threaded)

snapshots <- function(data,      # a data frame
                      vertex_a,  # first vertex. if pmi is calculated as weight, this is used as the feature column
                      vertex_b,  # second vertex. if pmi is caclulated as weight, this is used as the item column
                      time,      # time indicator to slice into. correct time slices need to be provided in the data frame (e.g. day, week, month...), is used as-is
                      directed = FALSE, # should the graph passed to the community detection be directed? If TRUE, then vertex_a -> vertex_b
                      pmi_weight = TRUE, # should the PMI be calculated for the slices and used as weights? if true make sure to specify vertex a and b correctly. Effectively projects a bipartite graph to monopartite
                      output = c("metrics", "networks"), # should metrics for each snapshot or the networks itself be the output? page_rank and community only work for metrics. If pmi_weight = T, this outputs the pmi-weighted network for each snapshot as a dataframe
                      page_rank = TRUE, # should the pagerank for nodes in each snapshot be calculated? Only if output = "metrics"
                      degree = FALSE,
                      community = TRUE, # should communities be calculated? if yes, specify a community function (default is Leiden). Only if output = "metrics
                      community_function = cluster_leiden, # provide the community detection function here, as provided by the igraph package (other functions are untested!). Only if output = "metrics
                      seed = NULL, # fixes RNG issues in parallelization. NULL only supresses warnings!
                      ...) { # ... to pass on arguments to algorithm, e.g. objective_function or resolution_paramter for cluster_leiden
  
  require(tidyverse)
  require(igraph)
  require(data.table)
  require(furrr)
  require(widyr)
  
  output <- match.arg(output)
  
  
  timeframes <-  split(as.data.table(data), by = time)
  
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
            select({{ vertex_a }}, {{ vertex_b }}) %>%
            pairwise_pmi_(feature =  {{vertex_a}}, item = {{vertex_b}}, sort = F) %>% rename(weight = pmi) %>% # calculate PMI as weight (use pairwise_pmi_() avoid problems with column specification)
            graph_from_data_frame(directed = directed) # make igraph object for slice
        })
        
      } else {
        # unweighted if not pmi-weighted
        slice <-
          timeframe %>% as_tibble() %>%
          select({{ vertex_a }}, {{ vertex_b }}) %>%
          graph_from_data_frame(directed = directed) # make igraph object for slice
      }
      
      if (output == "metrics") {
        slice_dat <- tibble(node = V(slice)$name,
                            time = timeframe %>% distinct(!!as.name(time)) %>% pull()) # get time from the timeframe
        
        try({
          # try() to avoid failure on empty slices
          
          if (community == TRUE) {
            communities <-
              community_function(slice, ...) # calculate communities
            slice_dat <-
              slice_dat %>% left_join(tibble(
                node = V(slice)$name,
                community = communities$membership
              ),
              by = "node")
          }
          
          if (page_rank == TRUE) {
            page_rank <- page_rank(slice)# calculate page rank
            slice_dat <-
              slice_dat %>% left_join(tibble(
                node = V(slice)$name,
                page_rank = page_rank$vector
              ),
              by = "node")
          }
          
          if (degree == TRUE) {
            degree <- degree(slice, mode = "total") # calculate degree
            slice_dat <- 
              slice_dat %>% left_join(tibble(
                node = V(slice)$name,
                degree = degree
              ),
              by = "node")
          }
          
        })
        
      }
      
      if (output == "networks") {
        slice_dat <- as_data_frame(slice, what = "edges") %>%
          mutate(time = timeframe %>% distinct(!!as.name(time)) %>% pull())
        
      }
      
      return(slice_dat)
      
    }

  
  output <-                    # mapping the snapshot function over each timeframe
    future_map(
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


