###### Twitter Stream #######

###### Preparations ######
library(rtweet)

setwd("....")

load("twittertoken.RDa")     # get a twitter token and put in your wd



## Twitter Stream         -------------------------------------
q = "..."  # your search pattern   

twitter_stream <- stream_tweets(q, timeout = 86400, token = twitter_token)  # set for 24 hour streams. adjust timeout for different length

stream_time <- substr(gsub(" ", "_", gsub("-|:", "", Sys.time())), 3, 13) # timestamp

stream_name <- paste("....", stream_time, sep= "")   # set stream name 

assign(stream_name, twitter_stream, envir = .GlobalEnv)

# save locally (in wd)
filename_stream <-  paste(stream_name,".RDa", sep= "")
save(list=stream_name, file = filename_stream)


