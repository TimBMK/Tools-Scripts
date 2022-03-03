## Function to split queries for academictwitteR into chunks adhering to the API limit of 1024 characters



# add_query_prefix from academictwitteR (https://github.com/cjbarrie/academictwitteR/blob/master/R/utils.R)
add_query_prefix <- function(x, prefix){
  q <- paste0(prefix, x)
  q <- paste(q, collapse = " OR ")
  q <- paste0("(",q,")")
  return(q)
}



query_splitter <- function(input, type = c("users", "conversations", "tweets"), batch, start_tweets, end_tweets, n, bind = F, data_path, bearer_token) {
  
  require(academictwitteR)
  require(stringr)
  
  type <- match.arg(type)
  
  # make index
  data <- data.frame(input = input, index = 1:length(input))
  
  # checkup loop (are the queries too long?)
  for (i in seq(1, length(data$input), batch)) {
    
    lookup <- na.omit(data$input[i:(i+(batch-1))])
    
    if (type == "users"){
      query <- build_query(users = str_trim(lookup)) # str_trim just in case
      if(nchar(query) > 1024) {
        stop(paste("Query too long:", nchar(query), "characters. Adjust batch size."))
      }
    } 
    
    if (type == "conversations"){
      query <- build_query(conversation_id = str_trim(lookup))
      
      # currently, academictwitteR's behaviour differs for covnersation_ids when building the query, hence the different evaluation. Might need fixing later (if changed)
      full_query <- paste("(", paste(query, collapse = " OR "), 
                          ")", sep = "")
      if(nchar(full_query) > 1024) {
        stop(paste("Query too long:", nchar(full_query), "characters. Adjust batch size."))
      }
    }
    #else {stop("Specify input. Currently only users are supported.")}
    # print(nchar(query))
    
    # if (type == "tweets"){
    #   query <- add_query_prefix(input, "ids:")
    # }
    # 
    
    if (type == "tweets"){
      query <- paste0("ids=", paste(input, collapse = ", "))
    }
    
  }
  
  
  output <- data.table()
  
  # actual loop
  for (i in seq(1, length(data$input), batch)) {
    
    # indicator
    cat(paste("\nBatch", ceiling(i/batch), "/", ceiling(length(data$input)/batch), 
              "\nInput row numbers", i, "to", (i+(batch-1)), "\n" ))
    
    lookup <- na.omit(data$input[i:(i+(batch-1))])
    
    if (type == "users"){
      query <- build_query(users = str_trim(lookup))} # str_trim just in case
    
    if (type == "conversations"){
      query <- build_query(conversation_id = str_trim(lookup))
    }
    
    
    get_all_tweets(query = query, start_tweets = start_tweets, 
                   end_tweets = end_tweets, n = n, 
                   data_path = data_path, bind_tweets = F,
                   bearer_token = bearer_token)
    
  }
  
  if (bind == T) {
    
    output <- bind_tweets(data_path = data_path, output_format = "tidy")
    
    return(output)
  }
  
}










