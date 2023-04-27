## Function to split queries for academictwitteR into chunks adhering to the API limit of 1024 characters



# # add_query_prefix from academictwitteR (https://github.com/cjbarrie/academictwitteR/blob/master/R/utils.R)
# add_query_prefix <- function(x, prefix){
#   q <- paste0(prefix, x)
#   q <- paste(q, collapse = " OR ")
#   q <- paste0("(",q,")")
#   return(q)
# }



query_splitter <- function(input, # input data, e.g. tweet or user IDs
                           is_retweet = NULL, is_reply = NULL, is_quote = NULL, is_verified = NULL, # additional parameters to be passed on if type = "users"
                           type = c("users", "conversations", "tweets"), # type of input data
                           batch_size, # number of items in a batch. May take a bit of trial and error to determine (function checks if it's longer than max_batchlength). 
                                       #    Set conservatively to avoid errors
                           max_batchlength = 1024, # maximum batch size. Currently, this is 1024 characters for search request (API restriction)
                           start_tweets, end_tweets, n = Inf, bind = F, data_path, bearer_token = get_bearer()) { # usual academitwitteR args
  
  require(academictwitteR)
  require(stringr)
  
  type <- match.arg(type)
  
  # make index
  data <- data.frame(input = input, index = 1:length(input))
  
  # checkup loop (are the queries too long?)
  for (i in seq(1, length(data$input), batch_size)) {
    
    lookup <- na.omit(data$input[i:(i+(batch_size-1))])
    
    if (type == "users"){
      query <- build_query(users = str_trim(lookup), # str_trim just in case
                           is_retweet = is_retweet, is_reply = is_reply, is_quote = is_quote, is_verified = is_verified) # addtional query terms
      if(nchar(query) > max_batchlength) {
        stop(paste("Query too long:", nchar(query), "characters. Adjust batch size."))
      }
    } 

    if (type == "conversations"){
      query <- build_query(conversation_id = str_trim(lookup))
      
      # currently, academictwitteR's behaviour differs for covnersation_ids when building the query, hence the different evaluation. Might need fixing later (if changed)
      # Note that this loop is only meant to emulate the query-build behaviour in academictwitteR and does not have any influence on the actual query
      full_query <- paste("(", paste(query, collapse = " OR "), 
                          ")", sep = "")
      if(nchar(full_query) > max_batchlength) {
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
  
  
  # actual loop
  for (i in seq(1, length(data$input), batch_size)) {
    
    # indicator
    cat(paste("\nBatch", ceiling(i/batch_size), "/", ceiling(length(data$input)/batch_size), 
              "\nInput row numbers", i, "to", (i+(batch_size-1)), "\n" ))
    
    lookup <- na.omit(data$input[i:(i+(batch_size-1))])
    
    if (type == "users"){
      query <- build_query(users = str_trim(lookup), # str_trim just in case
                           is_retweet = is_retweet, is_reply = is_reply, is_quote = is_quote, is_verified = is_verified)} # addtional query terms
    
    if (type == "conversations"){
      query <- build_query(conversation_id = str_trim(lookup))
    }
    
    try(
    get_all_tweets(query = query, start_tweets = start_tweets, 
                   end_tweets = end_tweets, n = n, 
                   data_path = data_path, bind_tweets = F,
                   bearer_token = bearer_token)
    )
  }
  
  # binding (if asked for). Note that this binds all .jsons in the specified folder, not only the the output of the split query. This is only a convenience option!
  if (bind == T) {
    
    output <- bind_tweets(data_path = data_path, output_format = "tidy")
    
    return(output)
  }
  
}





# query <- ""
# 
# for (user in users$user_id) {
#   while (nchar(query) +  nchar(paste0("from:", user, " OR ", "()")) < 1024) {
#     
#     if (nchar(query) > 0) {
#       query <- paste(query, paste0("from:", user), sep = " OR ")
#     } else {
#       query <- paste0(query, paste0("from:", user))
#     }
#     
#   }
#   
# }
#
# paste0("(", query, ")")






