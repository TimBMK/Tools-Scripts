#### Combine Twitter Data from rtweet Scrapes ####

library(data.table)

setwd("...") # set wd with your data



## RAM efficient combiner for large amounts of seperate datasets
pattern = "..." # add pattern that serves as identifier for files frmo your scrape here. e.g. "covid19data*". 

filenames <- list.files(pattern = paste0(pattern,"*")) # "*" to indicate differences in datanames
 
tweets <- data.table()

gc()


# batched (way faster than singles since less loop growing) 
batch <- 10 # sequence into batchses (here: 10)

system.time(
  for (i in seq(1, length(filenames), batch)){  
    files <- na.omit(filenames[i:(i+(batch-1))]) # loop through the batched files to load, omit na for leftover batches
    for (j in 1:length(files)) {
      load(files[j])
      print(paste((i+j-1), "/", length(filenames), ":", files[j]))} # indicates current dataset as number of total datasets
    tweets <- rbindlist(mget(c(ls(pattern=pattern), ls(pattern="tweets"))))   # add new datasets
    print("- batch combined -")
    rm(list=ls(pattern=pattern))  # remove data
    tweets <- unique(tweets, fromLast = T, by="status_id")  #remove duplicate tweets in data
  }
)

gc()

assign(pattern, tweets, envir = .GlobalEnv)
rm(tweets)
save(list=pattern, file = paste0(pattern,"_tweets_full.RDa"))


