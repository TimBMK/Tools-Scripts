###### Twitter Scrape: how-to#######

###### Preparations ######
install.packages("rtweet")
library(rtweet)

# Working directory #
setwd("...")    # set working directory (in "") (ordner muss existieren)


# keys und secrets via: https://developer.twitter.com/en/docs/basics/authentication/guides/access-tokens

TwitterTor_accesstoken <-  "..."        # keys einsetzen (anstatt ... - wichtig: in "")
TwitterTor_accesssecret <-  "..."
TwitterToR_twitterkey <- "..."
TwitterToR_twittersecret <- "..."

                                        # lokal abspeichern ist praktisch für später, aber muss nicht
save(TwitterToR_twitterkey,
     TwitterToR_twittersecret,
     TwitterTor_accesssecret,
     TwitterTor_accesstoken,
     file = "rkeys.RDa") # <--- this is where your keys are locally stored!

load("rkeys.RDa")       # <--- diesen Befehl brauchst du, wenn du deine keys in einer späteren session wieder in R laden willst (wichtig: working directory vorher setzen)




## Twitter Setup          -------------------------------


# token aus den keys (s.o.) erstellen 
twitter_token <- create_token(
  app = "rtweet_tokes",
  TwitterToR_twitterkey,
  TwitterToR_twittersecret)

                                      # lokal abspeichern macht sinn, dann musst du den token nicht in jeder session neu erstellen
save(twitter_token, file = "twittertoken.RDa")

load("twittertoken.RDa")    #Ladebefehl für spätere Sessions



## Twitter Search         -------------------------------------

library(rtweet)
# Search query mit Hashtags (.o.ä.)
q = "..."             

  # nach Muster "#BTW OR #BTW17 OR #Bundestagswahl"
  # Wichtig:  Muster nach Twitter query!
  # für genauere Infos zur query Twitter API doc
  # für schnelle info, wie man eine query zu einem spezifischen Begriff o.ä. erstellt, kannst du einfach auf Twitter.com im browser suchen und im browserfeld nachschauen (q=...)
                
twitter_search <- search_tweets(q, n = 17000, token = twitter_token, retryonratelimit = TRUE)  

#help(search_tweets)  # doc für search_tweets befehl

# es bietet sich an, die searches mit daten im Format yyyy-mm-dd-hh-mm zu versehen, wenn du händisch mehrere suchen druchführst

time <- substr(gsub(" ", "_", gsub("-|:", "", Sys.time())), 3, 13) 


name <- paste("...",time, sep= "")    # Name der Suche statt ...
assign(name, twitter_search, envir = .GlobalEnv)

# lokal speichern 
filename = paste(name,".RDa", sep= "")
save(list=search_name, file = filename)


## Als Funktion (super easy use)-------
Twitter_Search <- function(q, name){
  tweets <- search_tweets(q, n = 17000, token = twitter_token)
  systime <- substr(gsub(" ", "_", gsub("-|:", "", Sys.time())), 3, 13) 
  search_name <- paste(name, systime, sep= "")
  assign(search_name, tweets, envir = .GlobalEnv)
}
  # Sucht die in q (s.o.) spezifizierten tweets, gibt ein nach der Suche (name) und der aktuellen Zeit benanntes Dataframe mit den Ergebnissen aus. Speichert diese jedoch nicht lokal ab.
  # Benötigt twitter_token und das rtweet package



## Searches zusammenführen  ----------------

# combine
TwitterData <- mapply(c, name_yyyy-mm-dd-hh-mm, name_yyyy-mm-dd-hh-mm, name_yyyy-mm-dd-hh-mm, SIMPLIFY = F)  # für name_yyyy-mm-dd-hh-mm die jeweiligen searches einfügen (beleibig viele); vorher mit load("name_yyyy-mm-dd-hh-mm.RDa") in R laden, falls nicht drin


## make Dataframe ##
TwitterData <- as.data.frame(TwitterData, stringsAsFactors=F)
View(TwitterData)

# check for and delete duplicates (unique)
TwitterData[duplicated(TwitterData),]%>%view()
#check for duplicates.some tweets seem to have gotten duplicated during the streams and/or the combining of searches and tweets
TwitterData <- unique(TwitterData) #delete duplicates

#save(TwitterData, file = "TwitterData.RDa")
save(TwitterData, file = "TwitterData.RDa")

