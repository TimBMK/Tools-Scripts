###### Twitter Search #######

###### Preparations ######
library(rtweet)

setwd("....")

load("twittertoken.RDa")     # get a twitter token and put in your wd



## Twitter Search         -------------------------------------

# Search query mit Hashtags (.o.ä.)
q = "..."        # your search pattern        

twitter_search <- search_tweets(q, n = 17000, token = twitter_token)  

# you should check search volume with a manual search before e.g. setting it up in a cronjob (how often do I have to scrape?)

search_time <- substr(gsub(" ", "_", gsub("-|:", "", Sys.time())), 3, 13) # timestamp

search_name <- paste("...", search_time, sep= "")   # set stream name 

assign(search_name, twitter_search, envir = .GlobalEnv)

# lokal speichern 
filename_search <-  paste(search_name,".RDa", sep= "")
save(list=search_name, file = filename_search)
