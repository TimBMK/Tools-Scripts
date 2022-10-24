# based on https://cran.r-project.org/web/packages/elastic/vignettes/elastic.html


library(tidyverse)
library(elastic)
library(jsonlite)

user <-  readline(prompt = "Enter  Username: ") # enter credentials in console
password <-  readline(prompt = "Enter Password: ") # user prompts for username and password, so the password is not visible in the script
host <-  readline(prompt = "Enter Server Address: ") # server address of your choice

conn <- connect(host = host,
        path="",
        port = 443,
        user = user,
        pwd = password,
        transport_schema = "https",
        errors = "complete")

# ping to see if the connection works
ping(conn)


# get data via the full text Search

result <- Search(conn, size = 3) # just get 3 tweets (no query)

tweets <- result$hits$hits # result is a nested list



# use raw json data to flatten the output and get a df

result_raw <- Search(conn, size = 3, raw = T)

result <- jsonlite::fromJSON(result_raw)

tweets <- jsonlite::flatten(result$hits$hits)

names(tweets)


# for search query syntax see https://www.elastic.co/guide/en/kibana/7.5/kuery-query.html

# search hashtags

result_raw <- Search(conn, q = "entities.hashtags.text : covid19", size = 10000, raw = T)

result <- jsonlite::fromJSON(result_raw)

tweets <- jsonlite::flatten(result$hits$hits)


# we can also tell the elastic package to return dataframes

result <- Search(conn, size = 3, asdf = T) # just get 3 tweets (no query)

tweets <- result$hits$hits # result is a dataframe


# search mentions

q <- "entities.user_mentions.screen_name : \"spdde\" or entities.user_mentions.screen_name : \"jusos\""

q <- "id_str : 1082968337825624064"

result_raw <- Search(conn, q = q, 
                     size = 1000, raw = T)

result <- jsonlite::fromJSON(result_raw)

tweets <- jsonlite::flatten(result$hits$hits)



result_raw <- Search(conn, q = "id_str : 1082968337825624064 and entities.user_mentions.name : spdde", 
                     size = 1000, raw = T)


## Aggregations

aggs <- '{
        "aggs": {
          "tweets-over-time": {
            "date_histogram": {
              "field": "created_at",
              "calendar_interval":"month"
            }
          }
        }
  }'



agg <- Search(conn, body = aggs, size = 0, asdf = T)

tweets <- agg$aggregations$`tweets-over-time`$buckets





### Get all data via Scrolled Search
####################################



body <- '{
      "_source": ["id_str", "created_at", "user.name"],
      "query": {
        "range": {
          "created_at": {
          "gte": "Tue Feb 23 00:00:00 +0000 2021"
          }
        }
      },
      "sort": [
        {"created_at": "asc"}
      ]
    }'


# full_scroll is a function that keeps scrolling (and retrievin results) until there are no more results to be downloaded / a limit is hit


full_scroll <- function(conn,  # connection to elastic
                        body,  # query body. set search query and return values here
                        list_length = 100, # nr of result dataframes stored and bound simultaneously. Optimum depends on total size of data
                        # set to NULL to deactivate (and bind everything in the end) 
                        size = 10000,         # number of results per Search (result DF size). Balance with list_length. Maximum = 10.000
                        time_scroll = "1h",   # length of scroll id kept alive (see Search() documentation)
                        limit = NULL) {       # maximum number of results (NULL for unlimited/everything)
  require(progress)
  require(elastic)
  require(data.table)
  
  # initial search, sets query
  res <- Search(conn, asdf = T, size = size, body = body, time_scroll = time_scroll)
  
  # limits
  if (is.null(limit)){        
    total <- res$hits$total$value
  }
  
  if (!is.null(limit)){
    if (res$hits$total$value > limit) {    # if the limit is higher than the actual hits, use maximum hits as ceiling
      cat(paste("Returning", limit, "out of", res$hits$total$value, "search results."))
      total <- limit 
    } else {
      total <- res$hits$total$value
      cat(paste("Limit is higher than available search results. Returning all", total,"search hits.\n"))
    }
  } 
  
  # progress bar to track download progress and total time elapsed
  pb <- progress_bar$new( 
    format = "  downloading [:bar] :current / :total | :elapsedfull",
    total = total, clear = FALSE, width= 60)
  
  out <- res$hits$hits
  hits <- 1
  list <- list()
  
  if (is.null(limit)){
    while (hits != 0) {
      res <- scroll(conn = conn, x = res$`_scroll_id`, asdf = T)
      hits <- length(res$hits$hits)
      if (hits > 0) {
        list[length(list)+1] <- list(res$hits$hits)     # make list of results to bind later
      }
      if (is.null(list_length)) {
        if (hits == 0) {
          out <- rbindlist(list, fill = T)
          list <- list()
        }
      } else {
        if (length(list)==list_length) {                           # bind results every x calls (~10 datapoints per call)
          out <- rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
        if (length(list) <= list_length & hits == 0) {             # bind leftovers
          out <- rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
      }
      count <- ifelse(length(list) != 0, nrow(out) + sum(sapply(list, nrow)), nrow(out))
      if (pb$finished != T) {
        pb$update(count/total)
      }
      Sys.sleep(1)
    }
  }
  
  if (!is.null(limit)){
    while ((hits != 0) & ((nrow(out)+nrow(res$hits$hits)) < limit)) {
      res <- scroll(conn = conn, x = res$`_scroll_id`, asdf = T)
      hits <- length(res$hits$hits)
      if (hits > 0) {
        list[length(list)+1] <- list(res$hits$hits)     # make list of results to bind later
      }
      if (is.null(list_length)) {
        if (hits == 0) {
          out <- rbindlist(list, fill = T)
          list <- list()
        }
      } else {
        if (length(list)==list_length) {                           # bind results every x calls (~10 datapoints per call)
          out <- rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
        if (length(list) <= list_length & hits == 0) {             # bind leftovers
          out <- rbindlist(c(list(out), list), fill = T)
          list <- list()
        }
      }
      count <- ifelse(length(list) != 0, nrow(out) + sum(sapply(list, nrow)), nrow(out))
      if (pb$finished != T) {
        pb$update(count/total)
      }
      Sys.sleep(1)
    }
  }
  
  return(out)
}


some_other_data <- full_scroll_2(conn, body = full_body, list_length = 100, size = 10000, time_scroll = "10h", limit = 1000000)
















