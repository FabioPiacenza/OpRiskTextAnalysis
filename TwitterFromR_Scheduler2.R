# ## install rtweet from CRANif it's not already
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

Accounts2 <- c("@FinancialNews", "@CBSNews", "@cnnbrk", "@FoxNews", # News agencies (deleted "@GoogleNews", just few tweets about Google)
              "@BBCWorld", "@BBCNews", "@bbcworldservice", "@BBCBreaking", # BBC news
              "@FinancialTimes", "@FT", "@ftlive", # Financial Times accounts
              "@business", "@Bloomberg", "@markets", "@BloombergTV", # Bloomberg accounts
              "@BloombergUK", "@opinion", "@BloombergLive", # Bloomberg accounts
              "@Reuters", "@ReutersWorld", # Reuters accounts
              "@RiskDotNet", "@RiskNet_REG", "@RiskNet_RM", "@RiskNet_COM", # Risk.net accounts
              "@RiskNet_AM", "@RiskNet_DER", "@RiskQuantum", # Risk.net accounts
              "@guardian", "@Independent", "@DailyMirror", # UK newspapers (Zammarchi-Romano)
              "@nytimes", "@washingtonpost", "@WSJ", # US newspapers (Zammarchi-Romano)
              "@TheEconomist") # Other UK newspapers


path <- file.path("W:/Twitter/data") # path to save data

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))

FileTweetsAccounts2 <- paste(file.path(path, paste0("TweetsAccounts2_", date, ".RData")))

FileTweetsAccounts2Id <- paste(file.path(path, paste0("since_id_vector_Accounts2.RData")))

FirstExtraction <- FALSE

if(FirstExtraction){

  TweetsAccounts2 <- get_timeline(user = Accounts2, n = Inf, type = "recent", lang = "en",
                                  include_rts = FALSE, retryonratelimit = TRUE)#,  token = token)

  # Save tweets
  save(TweetsAccounts2, file = FileTweetsAccounts2)
  
  # Save tweets IDs (string format)
  since_id_vector_Accounts2 <- TweetsAccounts2$id_str
  save(since_id_vector_Accounts2, file = FileTweetsAccounts2Id)

}else{
  
  load(file.path(path, paste0("since_id_vector_Accounts2.RData")))

  TweetsAccounts2 <- get_timeline(user = Accounts2, n = Inf, type = "recent", lang = "en",
                                  include_rts = FALSE, since_id = since_id_vector_Accounts2,
                                  retryonratelimit = TRUE)

  since_id_vector_Accounts2 <- c(since_id_vector_Accounts2, TweetsAccounts2$id_str)

  # Save tweets
  save(TweetsAccounts2, file = FileTweetsAccounts2)

  # Save tweets IDs (string format)
  save(since_id_vector_Accounts2, file = FileTweetsAccounts2Id)
  
}


