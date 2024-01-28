# Clean the Global Environment
# rm(list = ls())
# cat("\014")

# Load packages ####
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
library(readtext)
library(lexicon)
library(word2vec)
library(tictoc)
library(fastmatch)
library(dplyr)
library(chron)
library(plotly)
library(cluster)
library(factoextra)
library(openxlsx)
library(stringr)
library(cld2)
library(data.table)
library(stringr)
library(stringdist)
library(stringi)
library(rtweet)
library(lubridate)
library(umap)
library(uwot)
library(gender)
# install_genderdata_package()
library(irlba)

path <- file.path("W:\\TextAnalysis\\data\\")

# Load tweets ####
pathTwitter <- file.path("W:\\Twitter\\data\\")

# Log file (when run standalone) ####
if(!exists("begin_date")){
  sink_check <- TRUE
  date <- Sys.time()
  date <- as.character(date)
  date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
  sink(file = file.path(pathTwitter, paste0("Log_TwitterAnalysis_", date, ".Rout")))
}else{
  sink_check <- TRUE
}

print("Load tweets")
tic()

# Data to select ####
Accounts <- TRUE
KeyWords <- TRUE # FALSE # 
UpperLimit <- +Inf

if(Accounts==FALSE && KeyWords==FALSE)
  stop("Select at least one between Accounts and KeyWords")

# Dates for tweets selection (if not existing)
# Otherwise manage dates from Wrapper
if(!exists("begin_date")) {
  # begin_date <- "11/05/2023"
  begin_date <- "15/06/2023"
}else{
  begin_date <- paste0(substr(begin_date,9,10), "/", substr(begin_date,6,7), "/", substr(begin_date,1,4))
}
if(!exists("end_date")) {
  # end_date <- "11/07/2023"
  end_date <- "15/06/2023"
}else{
  end_date <- paste0(substr(end_date,9,10), "/", substr(end_date,6,7), "/", substr(end_date,1,4))
}


# Selection of relevant files
if(Accounts){
  FileNames_Accounts2 <- list.files(path = pathTwitter, pattern = "TweetsAccounts2_")
  TweetsDates_Accounts2 <- substr(FileNames_Accounts2, start = 17, stop = 26)  
}
if(KeyWords){
  FileNames_KeyWords <- list.files(path = pathTwitter, pattern = "TweetsKeyWords_")
  TweetsDates_KeyWords <- substr(FileNames_KeyWords, start = 16, stop = 25)
}

begin_date <- as.Date(begin_date, "%d/%m/%Y")
end_date <- as.Date(end_date, "%d/%m/%Y")

if(Accounts){
  FileNames_Accounts2_Selected <- FileNames_Accounts2[(TweetsDates_Accounts2 >= begin_date) & (TweetsDates_Accounts2 <= end_date)]
}
if(KeyWords){
  FileNames_KeyWords_Selected <- FileNames_KeyWords[(TweetsDates_KeyWords >= begin_date) & (TweetsDates_KeyWords <= end_date)]
}

if(Accounts){
  if(length(FileNames_Accounts2_Selected)>0){
    load(file = file.path(pathTwitter, FileNames_Accounts2_Selected[1]))
    # TweetsAccounts <- TweetsAccounts2
    TweetsAccounts <- TweetsAccounts2 %>% dplyr::select(created_at, id_str, text, full_text)
    if(length(FileNames_Accounts2_Selected)>1){
      for(i in 2:length(FileNames_Accounts2_Selected)){
        load(file = file.path(pathTwitter, FileNames_Accounts2_Selected[i]))
        # TweetsAccounts <- rbind(TweetsAccounts, TweetsAccounts2)
        TweetsAccounts <- rbind(TweetsAccounts, 
                                TweetsAccounts2 %>% dplyr::select(created_at, id_str, text, full_text))
      }
    }
    rm(TweetsAccounts2)
  }
}

if(KeyWords){
  load(file = file.path(pathTwitter, FileNames_KeyWords_Selected[1]))
  if(Accounts && (length(FileNames_Accounts2_Selected)>0)){
    # TweetsAccounts <- rbind(TweetsAccounts, TweetsKeyWords)
    TweetsAccounts <- rbind(TweetsAccounts, TweetsKeyWords %>% dplyr::select(created_at, id_str, text, full_text))
  }else{
    # TweetsAccounts <- TweetsKeyWords
    TweetsAccounts <- TweetsKeyWords %>% dplyr::select(created_at, id_str, text, full_text)
  }
  if(length(FileNames_KeyWords_Selected)>1){
    for(i in 2:length(FileNames_KeyWords_Selected)){
      load(file = file.path(pathTwitter, FileNames_KeyWords_Selected[i]))
      # TweetsAccounts <- rbind(TweetsAccounts, TweetsKeyWords)
      TweetsAccounts <- rbind(TweetsAccounts, TweetsKeyWords %>% dplyr::select(created_at, id_str, text, full_text))
    }
  }
}

TweetsAccounts <- as.data.frame(TweetsAccounts)

toc()


# Plot of tweets time series
if(begin_date != end_date){
  ts_plot(TweetsAccounts, "days") +
  labs(title = "Number of tweets per day", x = "Day", y = "Number of tweets") +
  theme_minimal()
}


# Selection of relevant tweets
print("Selection of relevant tweets")
tic()

if(KeyWords){
  first_begin_date <- as.Date("05/05/2023", "%d/%m/%Y")
  last_end_date <- as.Date("12/07/2023", "%d/%m/%Y")  
}else{
  first_begin_date <- as.Date("02/05/2023", "%d/%m/%Y")
  last_end_date <- as.Date("12/07/2023", "%d/%m/%Y")
}


TweetsAccounts_Selected <- TweetsAccounts[which((as.Date(TweetsAccounts$created_at) >= first_begin_date) &
                                            (as.Date(TweetsAccounts$created_at) <= last_end_date)),]

# Add column of extraction date
TweetsAccounts_Selected$extracted_at <- begin_date

# Plot of tweets time series
if(begin_date != end_date){
  ts_plot(TweetsAccounts_Selected, "days") +
    labs(title = "Number of tweets per day", x = "Day", y = "Number of tweets") +
    theme_minimal()
}

# Plot_ly of daily tweets
if(begin_date != end_date){
  title_plot <- paste0("Number of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot <- paste0("Number of tweets as of ", begin_date)
}
if(begin_date != end_date){
  TweetsAccounts_Selected.ByDay <- TweetsAccounts_Selected %>% 
    group_by(date=as.Date(created_at)) %>% 
    summarize(n = length(id_str))
  TweetsAccounts_Selected.ByDay %>% 
    plot_ly(x = ~ date) %>% 
    add_lines(y = ~ n) %>%
    layout(
      xaxis = list(
        title = "Day"),
      yaxis = list(
        title = "Number of tweets"),
      title = title_plot)
}

# Constraint on the number of tweets
if(UpperLimit != Inf){
  TweetsAccounts_Selected <- TweetsAccounts_Selected[1:UpperLimit,]
}

tweets <- TweetsAccounts_Selected$text

toc()


# Clean the Text ####
print("Clean the tweets")
tic()

# https://rstudio-pubs-static.s3.amazonaws.com/286190_fbd48f12527e41ecaf45437beec599df.html
# Here we pre-process the data in some standard ways. I'll post-define each step
tweets <- iconv(tweets, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
tweets <- tolower(tweets)  # Make everything consistently lower case
# tweets <- gsub("rt", " ", tweets)  # Remove the "RT" (retweet) so duplicates are duplicates
tweets <- gsub("@\\w+", " ", tweets)  # Remove user names (all proper names if you're wise!)
tweets <- gsub("http.+ |http.+$", " ", tweets)  # Remove links
tweets <- gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets <- gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets <- gsub("amp", " ", tweets)  # "&" is "&amp" in HTML, so after punctuation removed ...
tweets <- gsub("^ ", "", tweets)  # Leading blanks
tweets <- gsub(" $", "", tweets)  # Lagging blanks
tweets <- gsub(" +", " ", tweets) # General spaces (should just do all whitespaces no?)

# Get rid of duplicates
TweetsAccounts_Selected$text <- tweets
TweetsAccounts_Selected <- TweetsAccounts_Selected[!duplicated(tweets),]
tweets <- TweetsAccounts_Selected$text
length(tweets) 
length(unique(tweets))  

toc()


# Corpus structure ####
print("Tokenizing the tweets")
tic()
Description <- tweets
Description_corpus <- corpus(Description)

# Assign Event IDs as description names
names(Description_corpus) <- TweetsAccounts_Selected$id_str

# Tokenizing text and creation of the Document-feature matrix (dfm)
toks <- tokens(Description_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, 
               remove_separators = T, remove_url = T)
typesnum <- grep("[[:digit:]]", types(toks), value = TRUE) # replace the digit number from 0 to 9 with white space
toks <- tokens_replace(toks, typesnum, gsub("[[:digit:]]", "\\s+", typesnum))
typeshyphens_end <- grep(".-$", types(toks), value = TRUE)
toks <- tokens_replace(toks, typeshyphens_end, gsub(".-$", "\\s+", typeshyphens_end))
strange_character <- grep("\\+s+", types(toks), value = TRUE)
toks <- tokens_replace(toks, strange_character, gsub("\\+s+", "\\s", strange_character))
# https://search.r-project.org/CRAN/refmans/lexicon/html/hash_lemmas.html
toks <- tokens_replace(toks, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
typeplus <- grep("[+]", types(toks), value = TRUE)
typeTwoLetters <- substr(types(toks), start = 1, stop = 2)
toc()

print("Load dictionaries from OpRisk data")
tic()
load(file = file.path(path, "dictionary_list_2023-11-14_12-23.RData"))
dict <- dictionary_list$dict
dict_OrxTaxo <- dictionary_list$dict_OrxTaxo
dict_2gram <- dictionary_list$dict_2gram
dict_3gram <- dictionary_list$dict_3gram
dict_other <- dictionary_list$dict_other
toc()

print("Exclude stopwords")
tic()
# Read external stopwords
Stopwords <- fread(file = file.path(path,"Stopwords.csv"), header=TRUE, check.names = FALSE, colClasses = "character", blank.lines.skip = T)
Stopwords <- data.frame(Stopwords)

# Convert DataFrame to vector in R
OtherStopwords <- Stopwords[["OtherStopwords"]]

# Include additional stopwords
OtherAdditionalStopwords <- c("say", "get", "just", "like", "trump", "know", "don", "think", "biden", "want",
                              "election", "party", "never", "way", "look", "win", "thing", "call", "many", 
                              "let", "guy", "voter", "yes", "governmanent", "country", "really", "nothing",
                              "state", "keep", "lot", "little", "big", "brain", "enough", "doesn", "help",
                              "tell", "find", "great", "president", "fight", "political", "must", "american",
                              "republican", "happen", "high", "talk", "away", "ukraine", "joe", "public",
                              "family", "doj", "politician", "politic", "kandie", "philemon", "anc",
                              "minister", "congress", "democracy", "nation", "russia", "cover", "stand",
                              "house", "china", "tory", "bring", "involve", "clinton", "else", "action",
                              "open", "lol", "woman", "week", "behind", "fuck", "feel", "last", "show",
                              "point", "anyone", "please", "someone", "start", "believe", "life", "ever",
                              "able", "either", "neither", "wait", "become", "philemonkandiemustgo",
                              "speak", "maybe", "understand", "around", "love", "read", "thank", "hear", 
                              "sure", "seem", "move", "place", "include", "shit", "simply", "actaul", "ppl",
                              "ago", "kind", "easy", "small", "sorry", "okay", "ok", "history", "hillary",
                              "impeach", "wonder", "past", "today", "russian", "every", "allow", "hope", 
                              "likely", "democrat", "god", "america", "isn", "though", "remember", "next",
                              "anything", "everyone", "true", "truth", "kid", "wasn", "vote", "different",
                              "something", "literally", "story", "live", "silvio", "berlusconi", "play",
                              "game", "rishi", "sunak", "boris", "johnson", "vladimir", "putin", "fav")
OtherStopwords <- c(OtherStopwords, OtherAdditionalStopwords)

# Include proper names into the stopwords
sets <- data(package = "genderdata")$results[,"Item"]
data(list = sets, package = "genderdata")
StopwordsNames <- unique(kantrowitz$name)
OtherStopwords <- c(OtherStopwords, StopwordsNames)

# Integrate dictionary into the tokens
toks <- tokens_lookup(toks, dictionary = dict, exclusive = FALSE, capkeys = FALSE, case_insensitive=T)

# Remove stopwords from tokens (useful for n-grams selection analysis)
toks <- toks %>%
  tokens_remove(pattern = c(stopwords("en"), OtherStopwords, typeshyphens_end, typeplus, typeTwoLetters)) %>%
  tokens_remove(pattern = stopwords("it")) %>%
  tokens_remove(pattern = stopwords("de"))

toc()


print("Integrate n-grams")
tic()

# Count of tokens (total features)
sum(ntoken(toks))
# Count of types (unique tokens)
sum(ntype(toks))

# dfm of tokens
toks_dfm <- toks %>% dfm

# Frequency of tokens
toks_freq <- featfreq(toks_dfm)

# Total number of tokens
toks_n <- sum(toks_freq)

# Probability of tokens
toks_prob <- toks_freq / toks_n


# 2-grams tweets ####

# Minimum frequency for tokens (words and n-grams)
Min_2gram_Freq <- 5

# Look for relevant 2-grams
toks_2gram <- tokens_ngrams(toks, n = 2)

# Total number of 2-grams
toks_2gram_n <- sum(ntoken(toks_2gram))

# dfm of 2-grams
toks_2gram_dfm <- toks_2gram %>% dfm() 

# 2-grams with frequency at least Min_2gram_Freq
toks_2gram_dfm_trim <- toks_2gram_dfm %>% 
  dfm_trim(max_docfreq = NULL, min_termfreq = Min_2gram_Freq, termfreq_type = "count") 

# Remove duplicated 2-grams (e.g. legal_legal)
featsplit <- strsplit(featnames(toks_2gram_dfm_trim), "_")
same <- sapply(featsplit, function(y) {
  length(y) >= 2 & # it's a compound (ngram)
    length(unique(y)) == 1 # all elements are the same
})
toks_2gram_dfm_trim <- toks_2gram_dfm_trim[, !same]

# Frequency of 2-grams
toks_2gram_freq <- featfreq(toks_2gram_dfm_trim)

# Probability of 2-grams
toks_2gram_prob <- toks_2gram_freq / toks_2gram_n

# Frequency of 2-grams for independence (product of tokens probability)
toks_names_ind <- str_split(names(toks_2gram_prob), "_")
calc_prob_ind <- function(toks_names, toks_prob){
  prod(toks_prob[names(toks_prob) %in% toks_names])
}
toks_2gram_prob_ind <- sapply(toks_names_ind, calc_prob_ind, toks_prob) 
names(toks_2gram_prob_ind) <- names(toks_2gram_prob)

# Binomial test for selecting relevant 2-grams
alpha_binom <- 0.005
p_value_binom <- pbinom(toks_2gram_freq, toks_2gram_n, toks_2gram_prob_ind, lower.tail = FALSE)
length(toks_2gram_freq)
toks_2gram_freq <- toks_2gram_freq[p_value_binom < alpha_binom]
length(toks_2gram_freq)

# Dictionary for 2-grams
toks_2gram_list <- str_split(names(toks_2gram_freq), "_")
dict_2gram_list <- lapply(toks_2gram_list, paste, collapse=" ")
names(dict_2gram_list) <- names(toks_2gram_freq)
dict_2gram_tweets <- dictionary(dict_2gram_list)


# 3-grams tweets ####

# Minimum frequency for tokens (words and n-grams)
Min_3gram_Freq <- 5

# Look for relevant 3-grams
toks_3gram <- tokens_ngrams(toks, n = 3)

# Total number of 3-grams
toks_3gram_n <- sum(ntoken(toks_3gram))

# dfm of 3-grams
toks_3gram_dfm <- toks_3gram %>% dfm() 

# 3-grams with frequency at least Min_3gram_Freq
toks_3gram_dfm_trim <- toks_3gram_dfm %>% 
  dfm_trim(max_docfreq = NULL, min_termfreq = Min_3gram_Freq, termfreq_type = "count") 

# Remove duplicated 3-grams (e.g. legal_legal)
featsplit <- strsplit(featnames(toks_3gram_dfm_trim), "_")
same <- sapply(featsplit, function(y) {
  length(y) >= 3 & # it's a 3-gram
    length(unique(y)) <= 2 # there are, at least, 2 equal word in the 3-gram
})
toks_3gram_dfm_trim <- toks_3gram_dfm_trim[, !same]

# Frequency of 3-grams
toks_3gram_freq <- featfreq(toks_3gram_dfm_trim)

# Probability of 3-grams
toks_3gram_prob <- toks_3gram_freq / toks_3gram_n

# Frequency of 3-grams for independence (product of tokens probability)
toks_names_ind <- str_split(names(toks_3gram_prob), "_")
calc_prob_ind <- function(toks_names, toks_prob){
  prod(toks_prob[names(toks_prob) %in% toks_names])
}
toks_3gram_prob_ind <- sapply(toks_names_ind, calc_prob_ind, toks_prob) 
names(toks_3gram_prob_ind) <- names(toks_3gram_prob)

# Binomial test for selecting relevant 3-grams
alpha_binom <- 0.005
p_value_binom <- pbinom(toks_3gram_freq, toks_3gram_n, toks_3gram_prob_ind, lower.tail = FALSE)
length(toks_3gram_freq)
toks_3gram_freq <- toks_3gram_freq[p_value_binom < alpha_binom]
length(toks_3gram_freq)

# Dictionary for 3-grams
toks_3gram_list <- str_split(names(toks_3gram_freq), "_")
dict_3gram_list <- lapply(toks_3gram_list, paste, collapse=" ")
names(dict_3gram_list) <- names(toks_3gram_freq)
dict_3gram_tweets <- dictionary(dict_3gram_list)


# Integrate relevant n-grams ####

# Integrate n-gram dictionary from ORX taxonomy into the tokens
toks <- tokens_lookup(toks, dictionary = dict_OrxTaxo, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate 3-gram dictionary into the tokens
toks <- tokens_lookup(toks, dictionary = dict_3gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate 2-gram dictionary into the tokens
toks <- tokens_lookup(toks, dictionary = dict_2gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate other relevant n-grams and dictionaries
toks <- tokens_lookup(toks, dictionary = dict_other, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate 3-gram tweets dictionary into the tokens
toks <- tokens_lookup(toks, dictionary = dict_3gram_tweets, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate 2-gram tweets dictionary into the tokens
toks <- tokens_lookup(toks, dictionary = dict_2gram_tweets, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

toc()


# Seed words ####
print("Define seed words")
tic()

# Load seeded words
Seeds <- "OrxTaxonomy" # "UmapBased" # "InternalData" 

if(Seeds == "InternalData"){
  # load(file = file.path(path,"seeded_words_complete.RData"))
  load(file = file.path(path,"seeded_words_2023-11-14_01-22.RData"))
  seeded_words <- seeded_words_complete
}

if(Seeds == "UmapBased"){
  
  Fraud <- c("fraud",
             "tax_fraud",
             "commit_fraud")
  topic_Fraud <- as.factor(rep("01_Fraud", length(Fraud)))
  
  Theft <- c("theft",
             "auto_theft",
             "theft_auto",
             "rubbery")
  topic_Theft <- as.factor(rep("02_Theft", length(Theft)))
  
  Injury <- c("injury",
              "injury_prone")
  topic_Injury <- as.factor(rep("03_Injury", length(Injury)))
  
  Money_Laundering <- c("money_launder")
  topic_Money_Laundering <- as.factor(rep("04_Money_Laundering", length(Money_Laundering)))
  
  Corruption <- c("corruption",
                  "corrupt",
                  "bribery",
                  "bribe",
                  "lie_corruption",
                  "expose_corruption")
  topic_Corruption <- as.factor(rep("05_Corruption", length(Corruption)))
  
  Breach <- c("breach",
              "data_breach")
  topic_Breach <- as.factor(rep("06_Breach", length(Breach)))
  
  Fine <- c("fine",
            "fines",
            "pay_fine")
  topic_Fine <- as.factor(rep("07_Fine", length(Fine)))
  
  Compliance <- c("compliance",
                  "regulation",
                  "legal",
                  "claim")
  topic_Compliance <- as.factor(rep("08_Compliance", length(Compliance)))
  
  Damage <- c("damage",
              "cause_damage",
              "damage_cause",
              "injury_prone",
              "damage_control",
              "terrorism",
              "terrorist")
  topic_Damage <- as.factor(rep("09_Damage", length(Damage)))
  
  Cyber <- c("cyber",
             "cyber_attack",
             "cyber_security",
             "cybersecurity",
             "scam")
  topic_Cyber <- as.factor(rep("10_Cyber", length(Cyber)))
  
  Error <- c("error",
             "mistake",
             "error_message")
  topic_Error <- as.factor(rep("11_Error", length(Error)))
  
  seeded_words <- data.frame(topic = c(topic_Fraud, topic_Theft, topic_Injury, topic_Money_Laundering, topic_Corruption, 
                                       topic_Breach, topic_Fine, topic_Compliance, topic_Damage, topic_Cyber, topic_Error), 
                             word = c(Fraud, Theft, Injury, Money_Laundering, Corruption, Breach, Fine, Compliance,
                                      Damage, Cyber, Error))
}

if(Seeds == "OrxTaxonomy"){
  
  Fraud <- c("fraud", 
             "rubbery",
             "theft" 
             )
  topic_Fraud <- as.factor(rep("01_Fraud", length(Fraud)))
  
  Physical_Security <- c("damage", # Include "People"
                         "injury",
                         "employee",
                         "terrorism",
                         "terrorist")
  topic_Physical_Security <- as.factor(rep("02_Physical_Security", length(Physical_Security)))
  
  Processing_And_Execution <- c("error", # Include "Model"
                                "mistake")
  topic_Processing_And_Execution <- as.factor(rep("03_Processing_And_Execution", length(Processing_And_Execution)))
  
  Technology <- c( # Include "Business_Continuity"
                  "hardware",
                  "software", 
                  "IT_failure",
                  "business_continuity",
                  "technology",
                  "bug")
  topic_Technology <- as.factor(rep("04_Technology", length(Technology)))
  
  Conduct_And_Legal <- c("sanction", # Include "Regulatory_Compliance"
                         "legal",
                         "breach",
                         "compliance",
                         "regulation",
                         "fine",
                         "claim")
  topic_Conduct_And_Legal <- as.factor(rep("05_Conduct_And_Legal", length(Conduct_And_Legal)))
  
  Financial_Crime <- c(
                       "money_launder",
                       "corrupt",
                       "bribe" 
                       )
  topic_Financial_Crime <- as.factor(rep("06_Financial_Crime", length(Financial_Crime)))
  
  Third_Party <- c("third_party",
                   "outsourcing",
                   "outsource")
  topic_Third_Party <- as.factor(rep("07_Third_Party", length(Third_Party)))
  
  Information_Security <- c("data_breach",
                            "cyber",
                            "hack",
                            "scam")
  topic_Information_Security <- as.factor(rep("08_Information_Security", length(Information_Security)))

  seeded_words <- data.frame(topic = c(topic_Fraud, topic_Physical_Security,  
                                       topic_Processing_And_Execution, topic_Technology, 
                                       topic_Conduct_And_Legal, topic_Financial_Crime,
                                       topic_Third_Party, topic_Information_Security), 
                             word = c(Fraud, Physical_Security, Processing_And_Execution, 
                                      Technology, Conduct_And_Legal, Financial_Crime,
                                      Third_Party, Information_Security))
}

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(seeded_words, file = file.path(pathTwitter, paste0("seeded_words_tweets_", begin_date, "_",  date, ".RData")))

# Dictionary for seeded words
toks_seeded <- seeded_words$word
dict_seeded_list <- as.list(paste0("*", toks_seeded, "*"))
names(dict_seeded_list) <- toks_seeded 
dict_seeded_tweets <- dictionary(dict_seeded_list)

# Integrate seeded words dictionary into the tokens
toks <- tokens_lookup(toks, dictionary = dict_seeded_tweets, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

toc()


# Minimum frequency for tokens ####
print("Document-by-term matrix")
tic()
MinTermFreq <- 5
# MinTermFreq <- 3

Description_dfm <- toks %>% 
  dfm() %>%
  dfm_trim(max_docfreq = NULL, min_termfreq = MinTermFreq, termfreq_type = "count")

toc()


# Statistics and wordcloud ####
print("Statistics and wordcloud")
tic()

# Dimension of dfm
print("Dimension of dfm")
dim(Description_dfm)

# Use the `topfeatures()` function to inspect the top 100 most frequently occurring features in the dfm
print("Top 100 most frequently occurring features")
topfeatures(Description_dfm, 100)

# Are there n-grams?
print("First 100 n-grams")
topf <- names(topfeatures(Description_dfm, 100000))
head(topf[grep("_", topf)], 100)

myDf <- data.frame(Description_dfm@Dimnames[["features"]])

# Wordcloud 
textplot_wordcloud(Description_dfm, min_count = 20,adjust = 0.5, max_words = 150, rotation = 0, fixed_aspect = T, 
                   color = c('gray48','black','lightcoral','turquoise4', 'lawngreen', 'orange', 'green3', 'darkorchid', 'forestgreen', 'mediumblue', 'red1'))
if(begin_date != end_date){
  title(paste0("Wordcloud of tweets from ", begin_date, " to ", end_date))
}else{
  title(paste0("Wordcloud of tweets as of ", begin_date))
}

toc()


# Apply semantic adjustment ####
print("Apply semantic adjustment")
tic()

RecalcSemAdj <- TRUE # FALSE #  

if(RecalcSemAdj){

# Load word similarity matrix
load(file = file.path(path,"embFilter_distanceAll_2023-11-14_15-01.RData"))

thrSim <- 0.8

embFilter_distanceAll <- embFilter_distanceAll[embFilter_distanceAll$cosine > thrSim,]
dim(embFilter_distanceAll)
# 
head(embFilter_distanceAll)


Semantic_tfidf <- Description_dfm # TF

dim(Semantic_tfidf)

# rm(Description_dfm)
gc()

names_Semantic_tfidf <- dimnames(Semantic_tfidf)$features
n_rows <- dim(Semantic_tfidf)[1]
n_columns <- dim(Semantic_tfidf)[2]
gc()


# Select words in our dictionaries
embFilter_distanceAll <- embFilter_distanceAll[(embFilter_distanceAll$document1 %fin% names_Semantic_tfidf) &
                                                 (embFilter_distanceAll$document2 %fin% names_Semantic_tfidf),]
dim(embFilter_distanceAll)


#tic()
# ind_emb <- NULL
ind_emb <- rep(0, n_rows)
for(i in 1:n_rows){
  #print(i)
  if(sum((toks[[i]] %fin% embFilter_distanceAll$document1) & (toks[[i]] %fin% embFilter_distanceAll$document2)) > 0){
    ind_emb[i] <- 1
  }
}
#toc()

ind_emb <- which(ind_emb == 1)

#tic()
for(i in ind_emb){ # Run the loop on the subset of documents with terms included in embFilter_distanceAll (before it was 1:n_rows)
  #print(i)
  subSemantic_tfidf <- Semantic_tfidf[i,][,which(Semantic_tfidf[i,]>0)] # Added "," before "which" to avoid "as.matrix"
  names_subSemantic_tfidf <- names(subSemantic_tfidf)
  # filter here embFilter_distance with the words in document i
  subEmbFilter_distance <- embFilter_distanceAll[(embFilter_distanceAll$document1 %fin% names_subSemantic_tfidf) |
                                                   (embFilter_distanceAll$document2 %fin% names_subSemantic_tfidf),]
  if(nrow(subEmbFilter_distance)>0){
    #tic()
    print(subEmbFilter_distance)
    for(j in 1:n_columns){
      if(Semantic_tfidf[i,j]==0){
        cond1 <- subEmbFilter_distance$document1 %fin% names_Semantic_tfidf[j]
        cond2 <- subEmbFilter_distance$document2 %fin% names_Semantic_tfidf[j]
        cond12 <- (cond1) | (cond2)
        embDoc1 <- subEmbFilter_distance[which(cond12),]
        
        if(nrow(embDoc1)>0){
          maxCosine <- max(embDoc1$cosine)
          embDoc2 <- embDoc1[embDoc1$cosine == maxCosine,]
          pivot <- ifelse(as.character(embDoc2$document1)==dimnames(Semantic_tfidf)$features[j],as.character(embDoc2$document2),as.character(embDoc2$document1))
          embDoc2_tfidf <- subSemantic_tfidf[which(names(subSemantic_tfidf) %fin% pivot)]
          Semantic_tfidf[i,j] <- max(embDoc2_tfidf) * embDoc2$cosine[1]
        }
      }
    }
    #toc()
  }
}
#toc()

gc()
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_tfidf, file = file.path(pathTwitter,paste0("Semantic_tfidf_", begin_date, "_" , date, ".RData")))
gc()

}else{
  load(file = file.path(pathTwitter,"Semantic_tfidf_2023-10-25_09-28.RData"))  
}

toc()


# To skip semantic adjustment
# Semantic_tfidf <- Description_dfm 
# rm(Description_dfm)
# gc()

print("Include cleaned tweets (based on tokenization) and select non-zero rows")
tic()

# Convert tweet tokens to character object
CleanedText <- sapply(toks, paste, collapse = " \u000A\ ")

# Add cleaned texts into tweets data
TweetsAccounts_Selected$text_cleaned <- CleanedText

gc()


TweetsAccounts.Filter <- TweetsAccounts_Selected
dim(TweetsAccounts.Filter)
rm(TweetsAccounts_Selected)

# Exclude documents with all zeros
dim(Semantic_tfidf)
Semantic_tfidf.Filter <- Semantic_tfidf[row.names(Semantic_tfidf) %in% TweetsAccounts.Filter$id_str,]
dim(Semantic_tfidf.Filter)

toks <- toks[names(toks) %in% TweetsAccounts.Filter$id_str,]
length(toks)

gc()

# # To skip LSA
# TweetsAccounts.Filter <- TweetsAccounts_Selected
# Semantic_tfidf.Filter <- Semantic_tfidf

# Select non-zero rows
sums <- ntoken(Semantic_tfidf.Filter)
Semantic_tfidf.Filter <- dfm_subset(Semantic_tfidf.Filter, sums > 0)
TweetsAccounts.Filter <- TweetsAccounts.Filter[sums != 0,]
dim(Semantic_tfidf.Filter)
dim(TweetsAccounts.Filter)
# Normalize vectors
Normalize <- FALSE
if(Normalize){
  for (i in 1:nrow(Semantic_tfidf.Filter)) {
    Semantic_tfidf.Filter[i,] <- Semantic_tfidf.Filter[i,] / sqrt(sum(Semantic_tfidf.Filter[i,]^2))  
  }
}

gc()
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_tfidf.Filter, file = file.path(pathTwitter,paste0("Semantic_tfidf.Filter_", begin_date, "_", date, ".RData")))
gc()

toc()


# PCA #### 
# https://stats.stackexchange.com/questions/35185/dimensionality-reduction-svd-or-pca-on-a-large-sparse-matrix
print("PCA of tweets")
tic()

Semantic_tfidf.Filter.dim <- dim(Semantic_tfidf.Filter)
pca.dim.max <- 50
pca.dim <- min(c(pca.dim.max, Semantic_tfidf.Filter.dim))

#tic()
Semantic_tfidf.Filter.pca <- prcomp_irlba(Semantic_tfidf.Filter, n=pca.dim)
#toc()

gc()
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_tfidf.Filter.pca, file = file.path(pathTwitter,paste0("Semantic_tfidf.Filter.pca_", begin_date, "_", date, ".RData")))
gc()

# https://www.datacamp.com/tutorial/pca-analysis-r?utm_source=google&utm_medium=paid_search&utm_campaignid=19589720818&utm_adgroupid=157156373911&utm_device=c&utm_keyword=&utm_matchtype=&utm_network=g&utm_adpostion=&utm_creative=676136073281&utm_targetid=dsa-2218886984060&utm_loc_interest_ms=&utm_loc_physical_ms=1008463&utm_content=&utm_campaign=230119_1-sea~dsa~tofu_2-b2c_3-eu_4-prc_5-na_6-na_7-le_8-pdsh-go_9-na_10-na_11-na&gad_source=1&gclid=EAIaIQobChMIjeuF06mqggMVZjkGAB2i9QetEAAYASAAEgLeV_D_BwE
summary(Semantic_tfidf.Filter.pca)
# https://stats.stackexchange.com/questions/584174/interpretation-of-low-variance-in-pca#:~:text=If%20the%20variance%20is%20%22low,to%20using%20only%20that%20variable.
# https://github.com/satijalab/seurat/issues/4920
# https://www.displayr.com/principal-component-analysis-of-text-data/#:~:text=This%20post%20introduces%20our%20new,of%20loadings%20to%20facilitate%20interpretation.
# https://stats.stackexchange.com/questions/378824/applying-pca-first-two-components-explain-low-variance-but-have-high-data-sepa
# https://stats.stackexchange.com/questions/439078/how-is-explained-variance-in-sparse-pca-calculated

fviz_eig(Semantic_tfidf.Filter.pca, ncp = pca.dim, addlabels = TRUE)
# Graph of the variables
fviz_pca_var(Semantic_tfidf.Filter.pca, col.var = "black")


Semantic_tfidf.Filter.Matrix <- Semantic_tfidf.Filter.pca$x
Semantic_tfidf.Filter.Matrix.Loadings <- Semantic_tfidf.Filter.pca$rotation

# gc()
# date <- Sys.time()
# date <- as.character(date)
# date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
# save(Semantic_tfidf.Filter.Matrix, file = file.path(pathTwitter,paste0("Semantic_tfidf.Filter.Matrix_", date, ".RData")))
# gc()

Semantic_pca.Filter.docs1 <- Semantic_tfidf.Filter.Matrix[, 1]
Semantic_pca.Filter.docs2 <- Semantic_tfidf.Filter.Matrix[, 2]
Semantic_pca.Filter.features1 <- Semantic_tfidf.Filter.Matrix.Loadings[, 1]
Semantic_pca.Filter.features2 <- Semantic_tfidf.Filter.Matrix.Loadings[, 2]
rownames.Semantic_pca.Filter.docs <- rownames(Semantic_tfidf.Filter)
rownames.Semantic_pca.Filter.features <- colnames(Semantic_tfidf.Filter)

df_pca <- data.frame(x=Semantic_pca.Filter.docs1, y=Semantic_pca.Filter.docs2,
                     id_desc=paste(rownames.Semantic_pca.Filter.docs, TweetsAccounts.Filter$full_text,
                                   "\u000A\ CLEANED:", TweetsAccounts.Filter$text_cleaned))

df_pca_features <- data.frame(x=Semantic_pca.Filter.features1, y=Semantic_pca.Filter.features2,
                              id_features=paste(rownames.Semantic_pca.Filter.features))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_pca, file = file.path(pathTwitter, paste0("df_pca_NoCluster_", begin_date, "_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_pca_features, file = file.path(pathTwitter, paste0("df_pca_features_", begin_date, "_", date, ".RData")))

if(begin_date != end_date){
  title_plot_pca <- paste0("PCA of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot_pca <- paste0("PCA of tweets as of ", begin_date)
}

plot_ly(df_pca, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
  layout(title = title_plot_pca)

if(begin_date != end_date){
  title_plot_pca_features <- paste0("PCA terms of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot_pca_features <- paste0("PCA terms of tweets as of ", begin_date)
}

plot_ly(df_pca_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
  layout(title = title_plot_pca_features)

toc()


# LSA ####
print("LSA of tweets")
tic()

# Apply LSA to the PCA result
Semantic_lsa.Filter <- textmodel_lsa(as.dfm(Semantic_tfidf.Filter.Matrix), nd = 2)

Semantic_lsa.Filter.docs1 <- Semantic_lsa.Filter$docs[, 1]
Semantic_lsa.Filter.docs2 <- Semantic_lsa.Filter$docs[, 2]
Semantic_lsa.Filter.features1 <- Semantic_lsa.Filter$features[, 1]
Semantic_lsa.Filter.features2 <- Semantic_lsa.Filter$features[, 2]
rownames.Semantic_lsa.Filter.docs <- rownames(Semantic_lsa.Filter$docs)
rownames.Semantic_lsa.Filter.features <- rownames(Semantic_lsa.Filter$features)

gc()
rm(Semantic_lsa.Filter)
gc()

df_lsa <- data.frame(x=Semantic_lsa.Filter.docs1, y=Semantic_lsa.Filter.docs2,
                     id_desc=paste(rownames.Semantic_lsa.Filter.docs, TweetsAccounts.Filter$full_text,
                                   "\u000A\ CLEANED:", TweetsAccounts.Filter$text_cleaned))

df_lsa_features <- data.frame(x=Semantic_lsa.Filter.features1, y=Semantic_lsa.Filter.features2,
                              id_features=paste(rownames.Semantic_lsa.Filter.features))

gc()


date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_lsa, file = file.path(pathTwitter, paste0("df_lsa_NoCluster_", begin_date, "_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_lsa_features, file = file.path(pathTwitter, paste0("df_lsa_features_", begin_date, "_", date, ".RData")))

if(begin_date != end_date){
  title_plot_lsa <- paste0("LSA of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot_lsa <- paste0("LSA of tweets as of ", begin_date)
}

plot_ly(df_lsa, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
  layout(title = title_plot_lsa)

if(begin_date != end_date){
  title_plot_lsa_features <- paste0("LSA terms of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot_lsa_features <- paste0("LSA terms of tweets as of ", begin_date)
}

plot_ly(df_lsa_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
  layout(title = title_plot_lsa_features)

toc()


# Clean memory ####
print("Clean memory")
tic()
rm(TweetsAccounts)          
rm(TweetsKeyWords)                          
rm(toks_3gram_dfm)                         
rm(toks_3gram)                     
rm(toks_2gram_dfm)                         
rm(toks_2gram)
rm(ipums_usa)                             
rm(ssa_national)
rm(ssa_state)
rm(napp)
gc()
toc()


# UMAP ####
# https://plotly.com/r/t-sne-and-umap-projections/
# https://github.com/lmcinnes/umap/issues/320
# https://github.com/tkonopka/umap/issues/11

print("UMAP of tweets")
tic()

#tic()
Semantic_tfidf.Filter.umap = uwot::umap(Semantic_tfidf.Filter.Matrix, 
                                        n_neighbors = 15, 
                                        n_components = 2,
                                        n_epochs = NULL, 
                                        #pca = pca.dim, 
                                        init = "spectral")
#toc()

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_tfidf.Filter.umap, file = file.path(pathTwitter, paste0("Semantic_tfidf.Filter.umap_", begin_date, "_", date, ".RData")))

layout <- Semantic_tfidf.Filter.umap
layout <- data.frame(layout)
df_umap <- cbind(layout, 
                 id_desc=paste(TweetsAccounts.Filter$id_str, TweetsAccounts.Filter$full_text,
                               "\u000A\ CLEANED:", TweetsAccounts.Filter$text_cleaned)
)

df_umap <- as.data.frame(df_umap)

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_umap, file = file.path(pathTwitter, paste0("df_umap_NoCluster_", begin_date, "_", date, ".RData")))

if(begin_date != end_date){
  title_plot_umap <- paste0("UMAP of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot_umap <- paste0("UMAP of tweets as of ", begin_date)
}

plot_umap <- plot_ly(df_umap, x = ~X1, y = ~X2, 
                     text = ~id_desc, 
                     type = 'scatter', mode = 'markers') %>%
  layout(
    plot_bgcolor = "#e5ecf6",
    xaxis = list(
      title = "V1"),
    yaxis = list(
      title = "V2"),
    title = title_plot_umap)
plot_umap

gc()
rm(Semantic_tfidf.Filter.Matrix)
gc()

toc()


# Seeded Latent Dirichlet Allocation (Seeded LDA) ####

# For LDA
library(textmineR)
# For wordclouds and seeded LDA
library(topicmodels)
library(wordcloud)

# Topic Modeling settings
SeedWeight <- 10^300 
num.iterations_par <- 1000L
burn_par <- num.iterations_par / 2

# Number of residual topics
extra_topics <- 5 

# Total number of topics (seeded and non-seeded)
k <- length(unique(seeded_words$topic)) + extra_topics

alpha_par <- 50 / k 
seed <- 1L

# Weight of non-seeded words (default is 0.1)
deltaW <- 0.1 

print(paste0("Estimate seeded LDA as of ", begin_date))
tic()

ii <- jj <- c()

for(w in 1:nrow(seeded_words)){
  word <- seeded_words[w,]
  sw <- which(Semantic_tfidf@Dimnames[[2]] == word[,"word"])
  ii <- c(ii,rep(as.integer(word[,"topic"]),length(sw)))
  jj <- c(jj,sw)
}

ordering <- order(ii)

jj <- jj[ordering]
ii <- ii[ordering]

deltaS <- slam::simple_triplet_matrix(ii, jj, v = rep(SeedWeight, length(ii)),
                                      nrow = k, ncol = ncol(Semantic_tfidf))

# dtm <- convert(Semantic_tfidf, "topicmodels")
dtm <- convert(Semantic_tfidf.Filter, "topicmodels")

# For LDA, input matrix needs to contain integer entries
dtm$v <- round(dtm$v)

# gc()

LDA_verbose <- round(num.iterations_par / 100)
LDA_keep <- max(LDA_verbose / 100, 1)

# https://stackoverflow.com/questions/53728858/r-topicmodels-package-how-to-set-the-parameter-of-beta-eta-when-we-do-lda

#tic()

model <- topicmodels::LDA(x = dtm, 
                          k = k, 
                          method = "Gibbs", 
                          seedwords = deltaS,
                          control = list(alpha = alpha_par, 
                                         delta = deltaW, # called also eta (or beta); default = 0.1
                                         estimate.beta = TRUE, 
                                         best = TRUE, 
                                         seed = seed,
                                         verbose = LDA_verbose, 
                                         burnin = burn_par, 
                                         iter = num.iterations_par,
                                         thin = num.iterations_par, 
                                         keep = LDA_keep,
                                         prefix = getwd()))

#toc()


date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(model, file = file.path(pathTwitter, paste0("Model_", Seeds, "_", begin_date, "_", date, ".RData")))


# Traceplot
LDA_logLiks <- model@logLiks

ylim_Traceplot <- c(min(LDA_logLiks), max(LDA_logLiks))

LDA_logliks_ind <- seq(LDA_keep, burn_par+num.iterations_par, by = LDA_keep)
plot(LDA_logliks_ind, LDA_logLiks, xlab = "Iteration", ylab = "Log-likelihood", 
     pch = 16, 
     lty = 1,
     col = "blue", type = "l", # "b",
     ylim = ylim_Traceplot)
title(paste0("Traceplot of log-likelihood value as of ", begin_date))
abline(v = burn_par, lty = 2)

toc()


print(paste0("Topics probability as of ", begin_date))
tic()
topicmodels::terms(model, 10)
table(topicmodels::topics(model))

distinctive_words <- function(lda_results){
  # P[ j | word ] = P[ word | j ] * P[ j ]/ sum_{k=1}^K P[ word | k ] * P[ k ].
  # with P[ j ] = sum_{k=1}^K ( P[ j | session_k ] * P[ session_k ] ), where
  # - P[ session_k ] = 1/(# of sessions)
  # - P[ j | session_k ] is the output of LDA
  
  gamma <- lda_results@gamma
  beta <- lda_results@beta
  terms <- lda_results@terms
  p_s <- 1/nrow(gamma)
  p_j <- t(rep(1,nrow(gamma)) %*% (gamma*p_s))
  num <- exp(beta) * (p_j %*% t(rep(1,ncol(beta))))
  p_jw <-  num / ( rep(1,nrow(beta)) %*% (rep(1,nrow(beta)) %*% num ))
  colnames(p_jw) <- terms
  return(p_jw)
}

dw <- t(distinctive_words(model))
colnames(dw) <- as.character(1:k)

ProbTopics <- model@gamma
colnames(ProbTopics) <- as.character(1:k)
rownames(ProbTopics) <- model@documents

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(ProbTopics, file = file.path(pathTwitter, paste0("ProbTopics", "_", begin_date, "_", date, ".Rdata")))

toc()


Averaged_LDA <- FALSE # Not used tweets (too slow and not so relevant): keep it FALSE
Averaged_LDA_Loaded <- FALSE

if(Averaged_LDA){
  
  ProbTopics_Orig <- ProbTopics
  nrow_ProbTopics <- nrow(ProbTopics)

  # tic()
  # ind_avg <- rep(0, nrow_ProbTopics)
  # for(i_doc in 1:nrow_ProbTopics){
  #   print(i_doc)
  #   tf_doc <- Semantic_tfidf.Filter[i_doc,]
  #   tf_doc_equal <- textstat_dist(Semantic_tfidf.Filter, tf_doc, method = "euclidean")==0
  #   tf_doc_equal <- as.logical(tf_doc_equal)
  #   if(sum(tf_doc_equal) > 1){
  #     ind_avg[i] <- 1
  #   }
  # }
  # toc()

  for(i_doc in 1:nrow_ProbTopics){
    print(i_doc)
    tf_doc <- Semantic_tfidf.Filter[i_doc,]
    tf_doc_equal <- textstat_dist(Semantic_tfidf.Filter, tf_doc, method = "euclidean")==0
    tf_doc_equal <- as.logical(tf_doc_equal)
    tf_doc_sim <- dfm_subset(Semantic_tfidf.Filter, tf_doc_equal )
    ProbTopics_set <- ProbTopics_Orig[rownames(ProbTopics_Orig) %in% rownames(tf_doc_sim), , drop=F]
    ProbTopics[i_doc,] <- apply(ProbTopics_set, 2, mean)
  }
  
  date <- Sys.time()
  date <- as.character(date)
  date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
  save(ProbTopics, file = file.path(pathTwitter, paste0("AvgProbTopics", "_", begin_date, "_", date, ".Rdata")))
  
}

if(Averaged_LDA_Loaded){
  load(file = file.path(path, paste0("AvgProbTopics", ".Rdata")))
}


ProbWords <- t(exp(model@beta))
row.names(ProbWords) <- model@terms

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(ProbWords, file = file.path(pathTwitter, paste0("ProbWords", "_", begin_date, "_", date, ".Rdata")))


# Seeded topics
topic_vector <- as.character(unique(seeded_words$topic))
# Seeded and unseeded topics
if(extra_topics > 0){
  topic_vector <- c(topic_vector, paste0("Other_",formatC(1:(k - length(topic_vector)) , width=2, flag="0") ))
}


print(paste0("Wordcloud as of ", begin_date))
tic()

def.par <- par(no.readonly = TRUE)
for (topic in 1:k) {
  df <- data.frame(term = row.names(ProbWords), p = ProbWords[, topic])
  head(df[order(-df$p), ])
  
  layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
  par(mar = rep(0, 4))
  plot.new()
  text(x = 0.5,
       y = 0.5,
       paste0(topic_vector[topic], " as of ", begin_date)) 
  
  wordcloud(
    words = df$term,
    freq = df$p,
    scale = c(2, .5),
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}
par(def.par) 

toc()


# Get the top terms of each topic
print("Top terms of the topics")
top_terms <- GetTopTerms(phi = t(ProbWords), M = 10)
colnames(top_terms) <- topic_vector
print(top_terms)


print(paste0("Assign clusters to tweets as of ", begin_date))
tic()

# Select filtered toks
toks <- toks[names(toks) %in% rownames(ProbTopics),]

# Assign the prevalent topic to each tweet
Constrainted <- TRUE
nrow.ProbTopics <- nrow(ProbTopics)
if(Constrainted){
  seeded_words$IndTopic <- as.numeric(substr(seeded_words$topic, 1 , 2))
  ListTopics <- as.list(rep(0, nrow.ProbTopics))
  
  for(i in 1:nrow.ProbTopics){
    ListTopics[[i]] <- 1:k  
  }
  
  for(i in 1:nrow.ProbTopics){
    #print(i)
    temp <- which(seeded_words$word %fin% toks[[i]])
    if(length(temp)>0){
      ListTopics[[i]] <- seeded_words$IndTopic[temp]
    }
  }
  temp <- rep(0, nrow.ProbTopics)
  for(i in 1:nrow.ProbTopics){
    #print(i)
    temp[i] <- max(ProbTopics[i,ListTopics[[i]]])
  }

  model_cluster_vec <- rep(0, length(temp))
  for(j in 1:length(temp)) {
    # Identify topic with max probability
    ProbTopicsConstr <- ProbTopics[j, ListTopics[[j]], drop=F]
    model_cluster_vec[j] <- colnames(ProbTopicsConstr[, ProbTopicsConstr == temp[j], drop=F])[1]
  }
  
}else{
  temp <- apply(ProbTopics , 1, max)
  model_cluster_vec <- rep(0, length(temp))
  for(j in 1:length(temp)) {
    # Identify topic with max probability
    model_cluster_vec[j] <- names(ProbTopics[j, ][ProbTopics[j, ] == temp[j]])
  }
}


model_cluster_vec <- as.numeric(model_cluster_vec)
table(model_cluster_vec)
# Same assignments of function topics
# table(topicmodels::topics(model))==table(model_cluster_vec)
length(model_cluster_vec)

TweetsAccounts.Filter$Cluster <- model_cluster_vec

print("Number of tweets for each cluster")
TweetsClusters <- table(TweetsAccounts.Filter$Cluster)
TweetsClusters

toc()


# PCA by cluster ####
print("PCA by cluster")
tic()

df_pca <- data.frame(x=Semantic_pca.Filter.docs1, y=Semantic_pca.Filter.docs2,
                     id_desc=paste(TweetsAccounts.Filter$id_str, "\u000A\ ", 
                                   topic_vector[TweetsAccounts.Filter$Cluster], "\u000A\ ",
                                   TweetsAccounts.Filter$full_text,
                                   "\u000A\ CLEANED:", TweetsAccounts.Filter$text_cleaned),
                     cluster=topic_vector[TweetsAccounts.Filter$Cluster])

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_pca, file = file.path(pathTwitter, paste0("df_pca_", begin_date, "_", date, ".RData")))

if(begin_date != end_date){
  title_plot_pca <- paste0("PCA of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot_pca <- paste0("PCA of tweets as of ", begin_date)
}

plot_ly(df_pca, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers",
        color = ~cluster) %>%
  layout(title = title_plot_pca)

toc()


# LSA by cluster ####
print("LSA by cluster")
tic()

df_lsa <- data.frame(x=Semantic_lsa.Filter.docs1, y=Semantic_lsa.Filter.docs2,
                     id_desc=paste(TweetsAccounts.Filter$id_str, "\u000A\ ", 
                                   topic_vector[TweetsAccounts.Filter$Cluster], "\u000A\ ",
                                   TweetsAccounts.Filter$full_text,
                                   "\u000A\ CLEANED:", TweetsAccounts.Filter$text_cleaned),
                        cluster=topic_vector[TweetsAccounts.Filter$Cluster])

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_lsa, file = file.path(pathTwitter, paste0("df_lsa_", begin_date, "_", date, ".RData")))


if(begin_date != end_date){
  title_plot_lsa <- paste0("LSA of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot_lsa <- paste0("LSA of tweets as of ", begin_date)
}

plot_ly(df_lsa, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers",
                     color = ~cluster) %>%
  layout(title = title_plot_lsa)

toc()


# Save final results ####
print("Save final results")
tic()

TweetsAccounts.Filter$cluster <- topic_vector[TweetsAccounts.Filter$Cluster]

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(TweetsAccounts.Filter, file = file.path(pathTwitter, paste0("TweetsAccounts.Filter_", begin_date, "_", date, ".RData")))
save(TweetsClusters, file = file.path(pathTwitter, paste0("TweetsClusters_", begin_date, "_", date, ".RData")))
toc()


# Plots of daily tweets by cluster ####

# Plot of daily tweets by cluster
if(begin_date != end_date){
  TweetsAccounts.Filter %>% 
    group_by(cluster) %>% 
    ts_plot("days") +
    labs(title = "Number of tweets per day", x = "Day", y = "Number of tweets") +
    theme_minimal()
}


# Plot_ly of daily tweets by cluster
if(begin_date != end_date){
  title_plot <- paste0("Number of tweets by cluster from ", begin_date, " to ", end_date)
}else{
  title_plot <- paste0("Number of tweets by cluster as of ", begin_date)
}
if(begin_date != end_date){
  TweetsAccounts.Filter.ByDayCluster <- TweetsAccounts.Filter %>% 
    group_by(cluster, date=as.Date(created_at)) %>% 
    summarize(n = length(id_str))
  # summarize(n = length(id))
  TweetsAccounts.Filter.ByDayCluster %>% 
    plot_ly(x = ~ date) %>% 
    add_lines(y = ~ n, 
              color = ~ factor(cluster)) %>%
    layout(
      # plot_bgcolor = "#e5ecf6",
      xaxis = list(
        title = "Day"),
      yaxis = list(
        title = "Number of tweets"),
      title = title_plot)
}

# https://www.gbailey.uk/twitter_workshop/pt2_collection.html
# https://rstudio-pubs-static.s3.amazonaws.com/840813_25b920e3cb4a4b04978718bd4199b0d4.html


# UMAP by cluster ####
# UMAP (https://plotly.com/r/t-sne-and-umap-projections/)
print("UMAP by cluster")
tic()

df_umap <- cbind(layout, 
               id_desc=paste(TweetsAccounts.Filter$id_str, "\u000A\ ", 
                             topic_vector[TweetsAccounts.Filter$Cluster], "\u000A\ ",
                             TweetsAccounts.Filter$full_text,
                             "\u000A\ CLEANED:", TweetsAccounts.Filter$text_cleaned), #, sep = " - "),
               cluster=topic_vector[TweetsAccounts.Filter$Cluster]
               )

df_umap <- as.data.frame(df_umap)

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_umap, file = file.path(pathTwitter, paste0("df_umap_", begin_date, "_", date, ".RData")))

if(begin_date != end_date){
  title_plot_umap <- paste0("UMAP of tweets from ", begin_date, " to ", end_date)
}else{
  title_plot_umap <- paste0("UMAP of tweets as of ", begin_date)
}

plot_umap <- plot_ly(df_umap, x = ~X1, y = ~X2, 
               text = ~id_desc, 
               color = ~cluster, 
               type = 'scatter', mode = 'markers') %>%
  layout(
    plot_bgcolor = "#e5ecf6",
    xaxis = list(
      title = "V1"),
    yaxis = list(
      title = "V2"),
    title = title_plot_umap)
plot_umap

toc()


print("End of calculation")

if(sink_check){
  sink()
}



