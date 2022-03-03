### Connecting to GraphQL (and making requests) ###
###################################################

library(ghql)
library(jsonlite)
library(dplyr)
library(crul)
library(lubridate)
library(data.table)
library(progress)

# link to the GraphQL Server, here tunneled via local host
# requires VPN to Heidelberg (and tunneling)

link <- "http://127.0.0.1:5001/graphql"


# get token (requires login with email and password)

email = "tim.koenig@uni-hildesheim.de"

password = "12345678910"

graphQL_token <- function (link, email, password) {
  
  require(crul)
  require(jsonlite)
  
  client <- HttpClient$new(link, headers=list(`Content-Type`="application/json"))
  
  response <- client$post(body = paste0("{\"query\": \"mutation { login(input: { email: \\\"", email,
                                        "\\\", password: \\\"", password, "\\\" }) { token } }\" }"), 
                          encode = "json")
  if (response$status_code == 200) print("Connection OK") else print("Can't connect - check Tunnel")
  
  token <- fromJSON(response$parse(encoding = "UTF-8"))$data$login$token
  
  if (is.null(token)) print("No Token returned - check email / password") else return (token)
}

token <- graphQL_token(link, email, password)

# # Calls with HttpClient
#
# client <- HttpClient$new(url = link, 
#                        headers = list(Authorization = paste0("Bearer ", token),
#                                       `Content-Type`="application/json"))
# 
# client$post(body = "{\"query\": \"{ newsArticle(id: \\\"101\\\") { id } }\"}", encode = "json") # note the triple escape for ids
# 
# client$post(body = "{\"query\": \"{ newsArticles { totalCount } }\"}", encode = "json")
# 
# client$post(body = "{\"query\": \"{ newsArticleAuthors(orderBy: NATURAL, first: 10) { edges { node { person { lastName }}} } }\"}", encode = "json")
# 
# client$post(body = "{\"query\": \"{ mediaByNodeId(nodeId: \\\"10\\\") { id } }\"}", encode = "json")



# Make requests
# based on https://www.r-bloggers.com/2020/12/accessing-grahpql-from-r/

conn <- GraphqlClient$new(url = link, 
                          headers = list(Authorization = paste0("Bearer ", token)))

# offset if the next X (from first or last) are needed
query <- 'query {
  newsArticles(offset: 100000, first: 100000) {      
    nodes {
      id
      publishedAt
      url
      extractedAt
    }
  }
}'


new <- Query$new()$query('link', query)


result <- conn$exec(new$link)

df <- as.data.frame(fromJSON(result))

df$data.newsArticles.nodes.publishedAt <- as_date(df$data.newsArticles.nodes.publishedAt)

df$data.newsArticles.nodes.extractedAt <- as_date(df$data.newsArticles.nodes.extractedAt)

qry <- Query$new()

qry$query("x",'{
  people(first: 10) {
    edges {
      node {
        id
      }
    }
  }
}

')


res <- conn$exec(qry$queries$x)



## Looping through large queries

conn <- GraphqlClient$new(url = link, 
                          headers = list(Authorization = paste0("Bearer ", token)))



qry <- Query$new()

qry$query("count",'{
  newsArticles {
    totalCount
  }
}
')


count <- as.numeric(fromJSON(conn$exec(qry$queries$count))$data$newsArticles$totalCount)

batch <- 10000  # set size of batch here. for our graphQL, the limit per call is dependant on the data (10k to be safe)

data <- data.table()

pb <- progress_bar$new( 
  format = "  downloading [:bar] :current / :total | :elapsedfull",
  total = count, clear = FALSE, width= 60)

for (i in 1:ceiling(count / batch)){
  
  offset <- seq(from = 0, to = ceiling(count / batch)-1) * batch  # set the offset to loop through the database
  
  # graphQL is struggling with scientific notation for numbers (e.g. 1e+05), so format() must be used to avoid bad request errors
  query <-paste0("query {
                        newsArticles(offset: ", format(offset[i], scientific = F),", 
                        first: ", format(batch, scientific = F),") {
                          nodes {
                            id
                            publishedAt
                            url
                            extractedAt
                            text {
                              language
                            }
                          }
                        }
                      }")
  
  new <- Query$new()$query(paste0('call_',i), query)
  
  result <- as.data.frame(fromJSON(conn$exec(new[[paste0("call_", i)]])))
  
  data <- rbindlist(list(data, as.data.table(result)))
  
 # print(paste(i, "out of", ceiling(count / batch), "finished!"))

  if (pb$finished != T) {
    pb$update(nrow(data)/count) # can throw errors in the pb if the data was updated during the search (e.g. more articles than expected from the first count),
  }                             #   but does not stop the data collection
  
  Sys.sleep(2)  # resting time between requests not strictly necessary, but can avoid errors
  
}






######
# batched requests as function [currently not working, since the offset needs to get implemented in arbitrary queries]

batched_requests <- function(query, size, list_length, sleep) {
  require(data.table)
  require(ghql)
  require(progress)
  require(jsonlite)
  
  count_qry <- Query$new()
  
  count_qry$query("total",'{
  newsArticles {
    totalCount
    }
  }
  ')
  
  total <- as.numeric(fromJSON(conn$exec(total_qry$queries$count))$data$newsArticles$totalCount)
  
  pb <- progress_bar$new( 
    format = "  downloading [:bar] :current / :total | :elapsedfull",
    total = total, clear = FALSE, width= 60)
  
  data <- data.table()
  
  for (i in 1:ceiling(total / batch)){
    
    offset <- seq(from = 0, to = ceiling(total / size)-1) * size  # set the offset to loop through the database
    
    # graphQL is struggling with scientific notation for numbers (e.g. 1e+05), so format() must be used to avoid bad request errors
    query <-paste0("query {
                        newsArticles(offset: ", format(offset[i], scientific = F),", 
                        first: ", format(size, scientific = F),") {
                          nodes {
                            id
                            publishedAt
                            url
                            extractedAt
                            text {
                              language
                            }
                          }
                        }
                      }")
    
    new <- Query$new()$query(paste0('call_',i), query)
    
    result <- as.data.frame(fromJSON(conn$exec(new[[paste0("call_", i)]])))
    
    data <- rbindlist(list(data, as.data.table(result)))
    
    print(paste(i, "out of", ceiling(total / size), "finished!"))
    
    Sys.sleep(2)  # resting time not strictly necessary, but can avoid errors
    
    if (pb$finished != T) {
      pb$update(nrow(data)/total)
    }
    
  }
  
}










