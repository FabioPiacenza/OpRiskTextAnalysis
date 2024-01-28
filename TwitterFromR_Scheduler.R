# ## install rtweet from CRAN if it's not already
if (!requireNamespace("rtweet", quietly = TRUE)) {
  install.packages("rtweet")
}

# ## install dev version of rtweet from github
# remotes::install_github("ropensci/rtweet")

## install remotes package if it's not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## install httpuv package if it's not already
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

## install tidyverse package if it's not already
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

## install data.table package if it's not already
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

## install lubridate package if it's not already
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}


## load packages
library(rtweet)
library(tidyverse)
library(httpuv)
library(remotes)
library(data.table)

## store api keys
consumerKey <- ""
consumerSecret <- ""
accessToken <- ""
accessTokenSecret <- ""

appName <- "AppOpRiskAcademic"


## token
token <- create_token(
  app = appName,
  consumer_key = consumerKey,
  consumer_secret = consumerSecret,
  access_token = accessToken,
  access_secret = accessTokenSecret,
  set_renv = TRUE
)
get_token()


## import tweets

KeyWords <- c("breach", "damage", "injury", "fraud", "rubbery", "theft", 
              "\"business continuity\"", "\"hardware failure\"", "\"software failure\"", 
              "\"IT failure\"", "cyber", 
              "terrorism", "sanction", "bribery", "corruption", "fines",  
              "compliance", "regulators", "\"third party\"", "outsourcing",
              "error", "\"model error\"", "\"implementation error\"", 
              "\"operational risk\"", "\"money laundering\"")
KeyWordsString <- paste(KeyWords, collapse = " OR ")
nchar(KeyWordsString)


path <- file.path("W:/Twitter/data") # path to save data
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
FileTweetsKeyWords <- paste(file.path(path, paste0("TweetsKeyWords_", date, ".RData")))

FileTweetsKeyWordsId <- paste(file.path(path, paste0("since_id_vector_KeyWords.RData")))

FirstExtraction <- FALSE

if(FirstExtraction){

  TweetsKeyWords <- search_tweets(q = KeyWordsString, n = Inf, type = "recent", lang = "en", 
                                  include_rts = FALSE, retryonratelimit = TRUE)

  # Save tweets
  save(TweetsKeyWords, file = FileTweetsKeyWords)
  
  # Save tweets IDs (string format)
  since_id_vector_KeyWords <- TweetsKeyWords$id_str
  save(since_id_vector_KeyWords, file = FileTweetsKeyWordsId)

}else{
  
  load(file.path(path, paste0("since_id_vector_KeyWords.RData")))
  
  TweetsKeyWords <- search_tweets(q = KeyWordsString, n = Inf, type = "recent", lang = "en", 
                                  include_rts = FALSE, since_id = since_id_vector_KeyWords, 
                                  retryonratelimit = TRUE)
  
  since_id_vector_KeyWords <- c(since_id_vector_KeyWords, TweetsKeyWords$id_str)
  
  # Save tweets
  save(TweetsKeyWords, file = FileTweetsKeyWords)
  
  # Save tweets IDs (string format)
  save(since_id_vector_KeyWords, file = FileTweetsKeyWordsId)
  
}


