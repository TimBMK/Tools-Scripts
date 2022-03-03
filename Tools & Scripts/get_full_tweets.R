# Get full tweets for GESIS' covid19 Dataset

library(rtweet)

load("twittertoken.RDa")

setwd("D:\\academicCloud\\R\\EPINetz\\Covid19")


load("Tweetscovid_char.RData")
# 
# COV19 <- TweetsCOV19[TweetsCOV19$Timestamp >= "2019-12-31 01:0:00",] # from 2019-12-31 00:00:00 UTC (reduce time if needed)
# 
# ids <- COV19$TweetId   # you'll need the status_ids
# 
# save(ids, file = "cov19_ids.RDa") # save and load if you're going to run the script on a server

load("cov19_ids.RDa")

# test
# tweets <- lookup_tweets(ids[5855], token = twitter_token) 

# system.time(covid_tweets <- lookup_tweets(ids[1:90000], token = twitter_token))


batch <- 90000 # beware rate limit of Twitter API when batching (90.000 for lookup_statuses / lookup_tweets)

# batch <- 10
# 
# ids <- ids[1:93]

# run batched lookups to circumvent rate limit. returns an individual dataset for every 90.000 tweets to be combined later
for (i in 1:ceiling(length(ids) / batch)){
  batched <- ids[((i-1)*batch+1):min(length(ids),(i*batch))]
  tweets <- lookup_tweets(batched, token = twitter_token)
  name <- paste0("covid_tweets_",i)
  assign(name, tweets, envir = .GlobalEnv)
  save(list=name, file = paste0(name,".RDa"))
  print(paste(i,"of", ceiling(length(ids) / batch), "done"))
  rm(list=ls(pattern=name))
  Sys.sleep(900)     # sleep to reset rate limit (every 15 minutes)
}

