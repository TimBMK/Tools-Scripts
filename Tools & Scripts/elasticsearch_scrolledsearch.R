
# function to conveniently retrieve more than 10.000 results from elastic search databases. outputs a neat dataframe


full_scroll <- function(conn,  # connection to elastic
                        q = NULL, # Search query
                        body = list(),  # query body. set return values here
                        list_length = 100, # nr of result dataframes stored and bound simultaneously. Optimum depends on total size of data
                        # (set to NULL to deactivate (and bind everything in the end))
                        index = NULL,         # Search index
                        size = 10000,         # number of results per Search (result DF size). Balance with list_length. Maximum = 10.000
                        time_scroll = "1h",   # length of scroll id kept alive (see Search() documentation)
                        limit = NULL,         # maximum number of results (NULL for unlimited/everything)
                        ...) {                # additional arguments passed to elastic::Search()
  require(progress)
  require(elastic)
  require(data.table)
  
  if (!is.null(limit) && limit < size) {size <- limit} # set size of the results per search to limit if needed, so no more than the limit are returned
  
  # initial search, sets query
  res <- elastic::Search(conn, q = q, asdf = T, size = size, body = body, time_scroll = time_scroll, index = index, ...)
  
  # limits
  if (is.null(limit)){        
    total <- res$hits$total$value
  }
  
  if (!is.null(limit)){
    if (res$hits$total$value > limit) {    # if the limit is higher than the actual hits, use maximum hits as ceiling
      print(paste("Returning", limit, "out of", res$hits$total$value, "search results."))
      total <- limit 
    } else {
      print("Limit is higher than actual search results. Returning all search hits.")
      total <- res$hits$total$value
    }
  } 
  
  # progress bar to track download progress and total time elapsed
  pb <- progress::progress_bar$new( 
    format = "  downloading [:bar] :current / :total | :elapsedfull",
    total = total, clear = FALSE, width= 60)
  
  out <- res$hits$hits
  hits <- 1
  list <- list()
  
  if (is.null(limit)){
    while (hits != 0) {
      res <- elastic::scroll(conn = conn, x = res$`_scroll_id`, asdf = T)
      hits <- length(res$hits$hits)
      if (hits > 0) {
        list[length(list)+1] <- list(res$hits$hits)     # make list of results to bind later
      }
      if (is.null(list_length)) {
        if (hits == 0) {
          out <- data.table::rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
      } else {
        if (length(list)==list_length) {                           # bind results every x calls (~10 datapoints per call)
          out <- data.table::rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
        if (length(list) <= list_length & hits == 0) {             # bind leftovers
          out <- data.table::rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
      }
      count <- ifelse(length(list) != 0, nrow(out) + sum(sapply(list, nrow)), nrow(out))
      if (pb$finished != T) {
        pb$update(count/total)
      }
    }
  }
  
  if (!is.null(limit)){
    while ((hits != 0) & ((nrow(out)+nrow(res$hits$hits)) < limit)) {
      res <- elastic::scroll(conn = conn, x = res$`_scroll_id`, asdf = T)
      hits <- length(res$hits$hits)
      if (hits > 0) {
        list[length(list)+1] <- list(res$hits$hits)     # make list of results to bind later
      }
      if (is.null(list_length)) {
        if (hits == 0) {
          out <- data.table::rbindlist(list, fill = T)
          list <- list()
        }
      } else {
        if (length(list)==list_length) {                           # bind results every x calls (~10 datapoints per call)
          out <- data.table::rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
        if (length(list) <= list_length & hits == 0) {             # bind leftovers
          out <- data.table::rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
      }
      count <- ifelse(length(list) != 0, nrow(out) + sum(sapply(list, nrow)), nrow(out))
      if (pb$finished != T) {
        pb$update(count/total)
      }
    }
  }
  
  return(out)
}




# this version saves the bound data of every step (list length) - more stable for large datasets

full_scroll_save <- function(
  conn,  # connection to elastic
  q = NULL, # Search query
  body = list(),  # query body. set return values here
  file_name, # name of the file to write to
  delim = ",", # delimiter
  index = NULL,         # Search index
  size = 10000,         # number of results per Search (result DF size). Balance with list_length. Maximum = 10.000
  time_scroll = "1h",   # length of scroll id kept alive (see Search() documentation)
  limit = NULL,         # maximum number of results (NULL for unlimited/everything)
  ...) {                # additional arguments passed to elastic::Search()

  require(progress)
  require(elastic)
  require(data.table)
  require(vroom)

  if (file.exists(file_name)) stop("File already exists.") # else the data would be appended to the same file

  if (!is.null(limit) && limit < size) {size <- limit} # set size of the results per search to limit if needed, so no more than the limit are returned

  # initial search, sets query
  res <- elastic::Search(conn, q = q, asdf = T, size = size, body = body, time_scroll = time_scroll, index = index, ...)
  
  # limits
  if (is.null(limit)){
    total <- res$hits$total$value
  }

  if (!is.null(limit)){
    if (res$hits$total$value > limit) {    # if the limit is higher than the actual hits, use maximum hits as ceiling
      print(paste("Returning", limit, "out of", res$hits$total$value, "search results."))
      total <- limit
    } else {
      print("Limit is higher than actual search results. Returning all search hits.")
      total <- res$hits$total$value
    }
  }

  # progress bar to track download progress and total time elapsed
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :current / :total | :elapsedfull",
    total = total, clear = FALSE, width= 60)

  out <- res$hits$hits
  count <- nrow(out)
  
  # write first data with colnames
  vroom::vroom_write(x = out, file = file_name,
                     progress = FALSE,
                     delim = delim, append = FALSE)
  
  if (pb$finished != T) {
    pb$update(count/total)
  }
  
  hits <- 1
  
  if (is.null(limit)){
    while (hits != 0) {
      res <- elastic::scroll(conn = conn, x = res$`_scroll_id`, asdf = T)
      hits <- length(res$hits$hits)
      
      if (hits > 0) {
        out <- data.table::rbindlist(list(out[1,], res$hits$hits), # bind with last results for same cols, remove the one row from the last results
                                     fill = TRUE)[-1,]
          
        
        vroom::vroom_write(x = out, file = file_name,
                           progress = FALSE,
                           delim = delim, append = TRUE) ## append = keep writing into the same file
      } 
      
      count <- ifelse(hits != 0, count + nrow(out), count)
      if (pb$finished != T) {
        pb$update(count/total)
      }
    }
  }
  

  if (!is.null(limit)){
    while ((hits != 0) & (count < limit)) {
      res <- elastic::scroll(conn = conn, x = res$`_scroll_id`, asdf = T)
      hits <- length(res$hits$hits)
      
      if (hits > 0) {
        out <- data.table::rbindlist(list(out[1,], res$hits$hits), # bind with last results for same cols, remove the one row from the last results
                                     fill = TRUE)[-1,]
        
        
        vroom::vroom_write(x = out, file = file_name,
                           progress = FALSE,
                           delim = delim, append = TRUE) ## append = keep writing into the same file
      } 
      
      count <- ifelse(hits != 0, count + nrow(out), count)
      if (pb$finished != T) {
        pb$update(count/total)
      }
    }
  }
}
