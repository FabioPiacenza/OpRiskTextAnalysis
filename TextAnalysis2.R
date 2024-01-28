# Clean the Global Environment
# rm(list = ls())
cat("\014")

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
library(uwot)
library(irlba)


# Path for input/output ####
path <- file.path("W:\\TextAnalysis\\data\\")

# Log file ####
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
sink(file = file.path(path, paste0("Log_TextAnalysis2_", date, ".Rout")))

# Read data ####
print("Read data")

tic()

OpData <- fread(file = file.path(path,"GROUP_Notification_2022_Q4_C1702.csv"),
                colClasses = c(rep("character",5), rep("numeric",11), rep("character",4)),
                header=TRUE, sep=";",check.names = FALSE,blank.lines.skip = T)
OpData_Corep <- fread(file = file.path(path,"GROUP_Notification_2022_Q4_C1702_Corep.csv"),
                      colClasses = c(rep("character",5), rep("numeric",11), rep("character",4)),
                      header=TRUE, sep=";",check.names = FALSE,blank.lines.skip = T)

OpData <- as.data.frame(OpData)
OpData_Corep <- as.data.frame(OpData_Corep)

# Set column names
names(OpData)<- names(OpData) %>% str_remove(" -.*") %>% str_remove(" \\[.*")
names(OpData_Corep)<- names(OpData_Corep) %>% str_remove(" -.*") %>% str_remove(" \\[.*")

# Dataset dimension
dim(OpData)
colnames(OpData)[1] <- "ID"
dim(OpData_Corep)
colnames(OpData_Corep)[1] <- "ID"

# Remove whitespace from start and end of string
OpData$Description<-str_squish(OpData$Description)
Description <- OpData$Description

OpData_Corep$Description<-str_squish(OpData_Corep$Description)
Description_Corep <- OpData_Corep$Description

toc()


# Procedure to extract English descriptions ####
print("Extract English descriptions")
tic()
Description <- Description %>%
  str_replace_all("\\s+ \\s+", "\u000A \\") %>% #replace more than whitespace in a row with line feed (/n)
  str_replace_all("\\s+[-]\\s+", "\u000A \\") %>% # replace " - " with line feed (/n)
  str_replace_all("\\s+[-]+", "\u000A \\") %>% # replace " --" with line feed (/n)        
  str_replace_all("[-]+\\s+", "\u000A \\") %>% # replace "-- " with line feed (/n)
  str_replace_all("[.]", "\u000A \\") %>% # replace ". " with line feed (/n)
  str_replace_all("[,]", "\u000A \\") %>% # replace ", " with line feed (/n)
  str_replace_all("iv\\)", "\u000A \\") %>% # replace "iv)" with line feed (/n)      
  str_replace_all("iii\\)", "\u000A \\") %>% # replace "iii)" with line feed (/n)
  str_replace_all("ii\\)", "\u000A \\") %>% # replace "ii)" with line feed (/n)
  str_replace_all("i\\)", "\u000A \\") %>% # replace "i)" with line feed (/n)
  str_replace_all("\\(iv\\)", "\u000A \\") %>% # replace "(iv)" with line feed (/n)
  str_replace_all("\\(iii\\)", "\u000A \\") %>% # replace "(iii)" with line feed (/n)
  str_replace_all("\\(ii\\)", "\u000A \\") %>% # replace "(ii)" with line feed (/n)
  str_replace_all("\\(i\\)", "\u000A \\") %>% # replace "(i)" with line feed (/n)
  str_replace_all("\\:\\s+", "\u000A \\") %>%  # replace ": " with line feed (/n)
  str_replace_all("[|||]", "\u000A \\")  # replace "|||" with line feed (/n)

Description_Corep <- Description_Corep %>%
  str_replace_all("\\s+ \\s+", "\u000A \\") %>% #replace more than whitespace in a row with line feed (/n)
  str_replace_all("\\s+[-]\\s+", "\u000A \\") %>% # replace " - " with line feed (/n)
  str_replace_all("\\s+[-+]", "\u000A \\") %>% # replace " --" with line feed (/n)        
  str_replace_all("[-+]\\s+", "\u000A \\") %>% # replace "-- " with line feed (/n)
  str_replace_all("[.]\\s+", "\u000A \\") %>% # replace ". " with line feed (/n)
  str_replace_all("[,]\\s+", "\u000A \\") %>% # replace ", " with line feed (/n)
  str_replace_all("i\\)", "\u000A \\") %>% # replace "i)" with line feed (/n)
  str_replace_all("ii\\)", "\u000A \\") %>% # replace "ii)" with line feed (/n)
  str_replace_all("iii\\)", "\u000A \\") %>% # replace "iii)" with line feed (/n)
  str_replace_all("iv\\)", "\u000A \\") %>% # replace "iv)" with line feed (/n)
  str_replace_all("\\(i\\)", "\u000A \\") %>% # replace "(i)" with line feed (/n)
  str_replace_all("\\(ii\\)", "\u000A \\") %>% # replace "(ii)" with line feed (/n)
  str_replace_all("\\(iii\\)", "\u000A \\") %>% # replace "(iii)" with line feed (/n)
  str_replace_all("\\(iv\\)", "\u000A \\") %>% # replace "(iv)" with line feed (/n)
  str_replace_all("\\:\\s+", "\u000A \\") %>%  # replace ": " with line feed (/n)
  str_replace_all("[|||]", "\u000A \\")  # replace "|||" with line feed (/n)

# For plotly
OpData$Description_v3 <- Description
OpData_Corep$Description_v3 <- Description_Corep

Description_splitted <- str_split(Description, '\u000A')
Description_splitted_Corep <- str_split(Description_Corep, '\u000A')


ORX_Taxonomy <- fread(file = file.path(path,"ORX_Taxonomy.csv"),
                      colClasses = c(rep("character",3)),
                      header=TRUE, sep=";",check.names = FALSE,blank.lines.skip = T)

relevant_words_set <- c("chf",
                        "bulk",
                        "legal ", # additional space to avoid selecting "legale" or "legali"
                        #"dispute", # Discarded to avoid the Italian word
                        "disputes",
                        "Covid",
                        "anatocism",
                        "usur",
                        ORX_Taxonomy$Level_3_Risks)


# How it works detect_language
# https://github.com/cld2owners/cld2#readme

my_detect_language <- function(string.vector, relevant_words){
  CheckLang <- cld2::detect_language(string.vector)
  CheckRelWords <- sapply(tolower(relevant_words), grepl, tolower(string.vector), USE.NAMES = F)
  if(is.vector(CheckRelWords)) CheckRelWords <- t(as.matrix(CheckRelWords))
  CheckRelWords <- apply(as.matrix(CheckRelWords),1,sum)
  CheckLang[(CheckRelWords>0) & is.na(CheckLang)] <- 'en'
  return(CheckLang)
}

MyExtractEnglishSubstrings <- function(string.vector, relevant_words) {
  return(string.vector[which(my_detect_language(string.vector, relevant_words) == 'en')])
}


#tic()
Description_v1 <- lapply(Description_splitted, MyExtractEnglishSubstrings, relevant_words_set)
#toc()
Description_v2 <- sapply( Description_v1, paste0, collapse="")
OpData <- cbind(OpData,Description_v2)
OpData$Description_v2<-str_squish(OpData$Description_v2)

#tic()
Description_v1_Corep <- lapply(Description_splitted_Corep, MyExtractEnglishSubstrings, relevant_words_set)
#toc()
Description_v2_Corep <- sapply( Description_v1_Corep, paste0, collapse="")
OpData_Corep <- cbind(OpData_Corep, Description_v2_Corep)
OpData_Corep$Description_v2_Corep<-str_squish(OpData_Corep$Description_v2_Corep)

# Remove unused objects 
rm(Description_splitted)
rm(Description_splitted_Corep)
rm(Description_v1)
rm(Description_v1_Corep)

gc()

# Manual corrections
OpData$Description_v2[OpData$Description_v2 %in% "inserted on upload"] <- ""
OpData_Corep$Description_v2[OpData_Corep$Description_v2 %in% "inserted on upload"] <- ""

# Keep only the English descriptions
OpData <- OpData[!(OpData$Description_v2==""), ]
OpData_Corep <- OpData_Corep[!(OpData_Corep$Description_v2==""), ]

dim(OpData)
dim(OpData_Corep)


# Remove original description
OpData$Description <- NULL
names(OpData)[names(OpData)=="Description_v2"] <- "Description"
Description <- OpData$Description
Description <- tolower(Description)

OpData_Corep$Description <- NULL
names(OpData_Corep)[names(OpData_Corep)=="Description_v2"] <- "Description"
Description_Corep <- OpData_Corep$Description
Description_Corep <- tolower(Description_Corep)

toc()

print("Tokenizing text, and removing stop-words")
tic()

# Corpus structure
Description_corpus <- corpus(Description)
Description_corpus_Corep <- corpus(Description_Corep)

# Assign Event IDs as description names
names(Description_corpus) <- OpData$ID
names(Description_corpus_Corep) <- OpData_Corep$ID

# Tokenizing text and creation of the Document-feature matrix (dfm)
toks <- tokens(Description_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T, remove_url = T)
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

toks_Corep <- tokens(Description_corpus_Corep, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T, remove_url = T)
typesnum_Corep <- grep("[[:digit:]]", types(toks_Corep), value = TRUE) # replace the digit number from 0 to 9 with white space
toks_Corep <- tokens_replace(toks_Corep, typesnum_Corep, gsub("[[:digit:]]", "\\s+", typesnum_Corep))
typeshyphens_end_Corep <- grep(".-$", types(toks_Corep), value = TRUE)
toks_Corep <- tokens_replace(toks_Corep, typeshyphens_end_Corep, gsub(".-$", "\\s+", typeshyphens_end_Corep))
strange_character_Corep <- grep("\\+s+", types(toks_Corep), value = TRUE)
toks_Corep <- tokens_replace(toks_Corep, strange_character_Corep, gsub("\\+s+", "\\s", strange_character_Corep))
# https://search.r-project.org/CRAN/refmans/lexicon/html/hash_lemmas.html
toks_Corep <- tokens_replace(toks_Corep, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
typeplus_Corep <- grep("[+]", types(toks_Corep), value = TRUE)
typeTwoLetters_Corep <- substr(types(toks_Corep), start = 1, stop = 2)

# Define a specific dictionary
dict <- dictionary(list(anatocism = c("anat*", "antocism*"),
                        document= c("document*","dokument","dokumentation"),
                        dismiss= "dismis*",
                        credit= "credit*",
                        bankrupt=c("bankrupt*"),
                        bank=c("banka*","banker","bank's","bank","banco","banca","bancaalal","benk"), 
                        client=c("client*", "clents","clent","cliet","cliens+","customer*","custumer","custormer's","customiz","customes+","customer","costumer"),
                        account=c("account*,accournts","accounts.th","account",	"account.th",	"accountabl"),
                        admin=c("administer","administratione","administrativelabortax","administrator"),
                        availability=c("availability*","available"),
                        finance=c("finanacial","financ*","finance","financial","finanziar","finanzamt","finanz"),
                        invstement=c("investment","investor","investor-friendly"),
                        italy="ital*",
                        legal=c("suit","legal","law","lawsu","lawsuit","lawyer","lega","leg","legali","legally","legislation","legislative","ultra","ultralegal","ultra-legal","ultralgal","ultrlegal"),
                        person=c("person","personal","personale","personalizzati"),
                        procedure="proc*",
                        regulation=c("regular","regulate","regulation","regulator","regulatory","regulatoryaccounting"),
                        usury="usur*",
                        suitable=c("suitability","suitabilty","suitable"),
                        system=c("system*","sistem*"),
                        transfer=c("transfe*"),
                        ucs="ucs*",
                        ucl="ucl*",
                        misselling=c("mis-sel*","missel*"),
                        transaction=c("transa*"),
                        account=c("accou*"),
                        agreement =c("agre*"),
                        analytics=c("analy*"),
                        assessment=c("asses*"),
                        assignment=c("assign*"),
                        atm= "atm*",
                        attack="attac*",
                        benefit="benefic*",
                        business="busines*",
                        capital="capital*",
                        cash="cash*",
                        employee=c("employ*","emplye"),
                        claim="claim*",
                        company="company*",
                        counterpart=c("counterpart*","counterpart","counterpaty"),
                        deficit=c("deficites","deficit","deficient","deficiency","defficits","defficit"),
                        department=c("department","departmen","departement","departament"),
                        derivatives=c("deriv*","deritv"),
                        economic=c("economy","economics","economically","economical","economic","econom"),
                        error=c("err*","erp","eroneously","eroneous"),
                        fraud=c("fraud*","fraus+","frudulent"),
                        illegal=c("illeg*"),
                        management=c("manag*"),
                        payment=c("'paymt","paym*"),
                        unicredit="unicredit*",
                        chf_loan=c("chf housing loan*","chf indexed housing loan*","loan* in chf","chf bulk","chf loan*","consumer loan* *chf*","chf*libor","loan* in valute chf","chf currency loan*"),
                        portfolio="protfolio",
                        data="datum"),
                   tolower=T)

# Read external stopwords
Stopwords <- fread(file = file.path(path,"Stopwords.csv"), header=TRUE, check.names = FALSE, colClasses = "character", blank.lines.skip = T)
Stopwords <- data.frame(Stopwords)

# Convert DataFrame to vector in R
OtherStopwords <- Stopwords[["OtherStopwords"]]

# Integrate dictionary into the tokens
toks <- tokens_lookup(toks, dictionary = dict, exclusive = FALSE, capkeys = FALSE, case_insensitive=T)
toks_Corep <- tokens_lookup(toks_Corep, dictionary = dict, exclusive = FALSE, capkeys = FALSE, case_insensitive=T)

# Remove stopwords from tokens (useful for n-grams selection analysis)
toks <- toks %>%
  tokens_remove(pattern = c(stopwords("en"), OtherStopwords, typeshyphens_end, typeplus, typeTwoLetters)) %>%
  tokens_remove(pattern = stopwords("it")) %>%
  tokens_remove(pattern = stopwords("de"))

toks_Corep <- toks_Corep %>%
  tokens_remove(pattern = c(stopwords("en"), OtherStopwords, typeshyphens_end_Corep, typeplus_Corep, typeTwoLetters_Corep)) %>%
  tokens_remove(pattern = stopwords("it")) %>%
  tokens_remove(pattern = stopwords("de"))

toc()

print("Detect and integrate relevant n-grams")
tic()

# Count of tokens (total features)
sum(ntoken(toks))
# Count of types (unique tokens)
sum(ntype(toks))

# dfm of tokens
toks_dfm <- toks %>% dfm
toks_dfm_Corep <- toks_Corep %>% dfm

# Frequency of tokens
toks_freq <- featfreq(toks_dfm_Corep)
# Total number of tokens
toks_n <- sum(toks_freq)

# Probability of tokens
toks_prob <- toks_freq / toks_n


# 2-grams ####

# Minimum frequency for tokens (words and n-grams)
Min_2gram_Freq <- 5

# Look for relevant 2-grams
toks_2gram <- tokens_ngrams(toks_Corep, n = 2)

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
dict_2gram <- dictionary(dict_2gram_list)


# 3-grams ####

# Minimum frequency for tokens (words and n-grams)
Min_3gram_Freq <- 5

# Look for relevant 3-grams
toks_3gram <- tokens_ngrams(toks_Corep, n = 3)

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
dict_3gram <- dictionary(dict_3gram_list)


# n-grams from ORX taxonomy ####

toks_OrxTaxo_list <- str_split(ORX_Taxonomy$Level_3_Risks, " ")
dict_OrxTaxo_list <- lapply(toks_OrxTaxo_list, paste, collapse = " ")
names(dict_OrxTaxo_list) <- sapply(toks_OrxTaxo_list, paste, collapse = "_")

# Exclude n-grams that are already in 3-grams and 2-grams lists
dict_OrxTaxo_list <- dict_OrxTaxo_list[which(!dict_OrxTaxo_list %in% dict_3gram_list)]
dict_OrxTaxo_list <- dict_OrxTaxo_list[which(!dict_OrxTaxo_list %in% dict_2gram_list)]

dict_OrxTaxo <- dictionary(dict_OrxTaxo_list)


# Integrate relevant n-grams ####

# Integrate n-gram dictionary from ORX taxonomy into the tokens
toks_Corep <- tokens_lookup(toks_Corep, dictionary = dict_OrxTaxo, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)
toks <- tokens_lookup(toks, dictionary = dict_OrxTaxo, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate 3-gram dictionary into the tokens
toks_Corep <- tokens_lookup(toks_Corep, dictionary = dict_3gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)
toks <- tokens_lookup(toks, dictionary = dict_3gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate 2-gram dictionary into the tokens
toks_Corep <- tokens_lookup(toks_Corep, dictionary = dict_2gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)
toks <- tokens_lookup(toks, dictionary = dict_2gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Other relevant n-grams and dictionaries
dict_other <- dictionary(list(chf_loan="*chf_loan*",
                              anatocism="*anatocism*",
                              client_account=c("conto_corrente", "bank_ccount", "current_account"),
                              incident="incidente",
                              card_fraud_forge="fraud_forge_card",
                              criminal_event="criminal_eventevento_criminoso",
                              cash_withdrawals_fraud="relativia_prelievi_fraud",
                              card_frad_cloning=c("unicredit_card_clone", "clonazutilizzo_fraud_carta", "card_fraud_losscloning"),
                              skim_device_card=c("skim_device_card", "card_skim_device"),
                              former_employee_litigation=c("recuperato_all'ex_dipendente","all'ex_dipendente_legal", "concorrenzaimporto_recuperato_all'ex"),
                              demotion="demansionamento",
                              data="datum", # likely "datum" is due to lemmatization transforming data into datum
                              internal_fraud="*internal_fraud*",
                              advisor = c("*advisor*", "finance_promoter")),
                       tolower=T)

toks_Corep <- tokens_lookup(toks_Corep, dictionary = dict_other, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)
toks <- tokens_lookup(toks, dictionary = dict_other, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

dictionary_list <- list(dict=dict, dict_OrxTaxo=dict_OrxTaxo, 
                        dict_2gram=dict_2gram, dict_3gram=dict_3gram, dict_other=dict_other)

# Save dictionary_list
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(dictionary_list, file = file.path(path, paste0("dictionary_list_", date, ".Rdata")))


# rm(dict_2gram)
# rm(dict_2gram_list)
# rm(dict_3gram)
# rm(dict_3gram_list)
# rm(dict_OrxTaxo)
# rm(dict_OrxTaxo_list)
# rm(toks_2gram_list)
# rm(toks_2gram_dfm)
# rm(toks_2gram_dfm_trim)
# rm(toks_3gram_list)
# rm(toks_3gram_dfm)
# rm(toks_3gram_dfm_trim)
# rm(toks_Corep)
# rm(toks_dfm_Corep)

toc()

gc()



# Seeded words ####
# In case of dictionary creation for seeds (similar to tweet data)
# Not done for the thesis (keep it FALSE)

seeded_words_dictionary <- FALSE

if(seeded_words_dictionary){
  
  # ETs
  ET <- sort(unique(OpData_Corep$'Event Type'))
  
  # Select the ETs to be split
  ET.Filter <- ET[c(1, 2, 3, 4, 5, 6, 7)]
  
  # Initialize the data frame of seeded words
  seeded_words_df <- data.frame(topic = NULL,
                                      word = NULL)
  
  for(i in 1:length(ET.Filter)){
    if(ET.Filter[i] == "EL0100 - Internal Fraud"){
      Client_Account <- c("account", 
                          "client_bank",
                          "unauthorized"
                          )
      topic_Client_Account <- as.factor(rep("EL0101_Client_Account", length(Client_Account)))
      Unfaithfulness <- c("Unfaithfulness",
                          "branch",
                          "infedelta",
                          "embezzlement",
                          "employee",
                          "merchant",
                          "finance_agent",
                          "fineco" 
                          )
      topic_Unfaithfulness <- as.factor(rep("EL0102_Unfaithfulness", length(Unfaithfulness)))
      ATM_Fraud <- c("atm",
                     "cash",
                     "supply",
                     "empty",
                     "refill",
                     "banknote",
                     "vacation")
      topic_ATM_Fraud <- as.factor(rep("EL0103_ATM_Fraud", length(ATM_Fraud)))
      seeded_words <- data.frame(topic = c(topic_Client_Account, topic_Unfaithfulness, topic_ATM_Fraud), 
                                 word = c(Client_Account, Unfaithfulness, ATM_Fraud))
    }
    if(ET.Filter[i] == "EL0200 - External Fraud"){
      Card_Fraud <- c("card",
                      "transaction_steal_credit",
                      "data_transaction_unauthorized",
                      "payment_productservice_internet",
                      "productservice_internet_provider",
                      "internet_provider_abroad",
                      "execute_eea_investigation",
                      "transaction_execute_eea",
                      "claim_recover_client",
                      "investigation_claim_recover",
                      "eea_investigation_claim"
                      )
      topic_Card_Fraud <- as.factor(rep("EL0201_Card_Fraud", length(Card_Fraud)))  
      seeded_words <- data.frame(topic = c(topic_Card_Fraud),
                                 word = c(Card_Fraud))
    }
    if(ET.Filter[i] == "EL0300 - Employment Practices and Workplace Safety"){
      Work_Injury = c( 
                      "injury", 
                      "work", 
                      "road", 
                      "apartment")
      topic_Work_Injury <- as.factor(rep("EL0301_Work_Injury", length(Work_Injury)))
      Former_Employee_Litigation <- c("former_employee_litigation",
                                     "legal_procedure_non-competition",
                                     "dipendente_legal_procedure",
                                     "non-competitio")
      topic_Former_Employee_Litigation <- as.factor(rep("EL0302_Former_Employee_Litigation", 
                                                        length(Former_Employee_Litigation)))
      Demotion <- c("demotion", 
                   "compensatory")
      topic_Demotion <- as.factor(rep("EL0303_Demotion", 
                                      length(Demotion)))
      seeded_words <- data.frame(topic = c(topic_Work_Injury, topic_Former_Employee_Litigation, topic_Demotion),
                                 word = c(Work_Injury, Former_Employee_Litigation, Demotion))
    }
    if(ET.Filter[i] == "EL0400 - Clients, Products & Business Practices"){
      CHF_Loans <- c("chf",
                     "variable_interest_rate", 
                     "currency", 
                     "overpay", 
                     "house_loan", 
                     "interest_rate_consumer", 
                     "client_dispute_appliance",
                     "legal_dispute_client",
                     "client_benefit", 
                     "initiated_procedure_bank",
                     "procedure_bank_dispute", 
                     "bank_dispute_validity",
                     "dispute_validity_variable", 
                     "validity_variable_interest",
                     "variable_interest_rate",
                     "interest_rate_clause",
                     "rate_clause_validity", 
                     "clause_validity_currency",
                     "validity_currency_clause", 
                     "currency_clause_client",
                     "clause_client_claim", 
                     "client_claim_repayment",
                     "claim_repayment_overpay")
      topic_CHF_Loans <- as.factor(rep("EL0401_CHF_Loans", length(CHF_Loans)))
      Anatocism = c("anatocism", 
                    "usury",
                    "overdraft_interest_rate"
                    )
      topic_Anatocism <- as.factor(rep("EL0402_Anatocism", length(Anatocism)))
      
      Personal_Loan_Reimbursement = c("ucifin_branch_reimbursement",
                                      "branch_reimbursement_open", 
                                      "reimbursement_open_fee", 
                                      "open_fee_person",
                                      "fee_person_loan")
      topic_Personal_Loan_Reimbursement <- as.factor(rep("EL0403_Personal_Loan_Reimbursement", 
                                                         length(Personal_Loan_Reimbursement)))
      
      Derivatives_Misselling = c("derivative", 
                                 "finance_instrument_lack",
                                 "instrument_lack_information",
                                 "lack_information_client",
                                 "corporates_division_client",
                                 "division_client_claim",
                                 "client_claim_compensation",
                                 "miscounselling"  
                                 )             
      topic_Derivatives_Misselling <- as.factor(rep("EL0404_Derivatives_Misselling", 
                                                    length(Derivatives_Misselling)))
      
      Client_Account = c("account", 
                         "claim_damage"
      )             
      topic_Client_Account <- as.factor(rep("EL0405_Client_Account", 
                                            length(Client_Account)))
      
      seeded_words <- data.frame(topic = c(topic_CHF_Loans, 
                                           topic_Anatocism,
                                           topic_Personal_Loan_Reimbursement, 
                                           topic_Derivatives_Misselling,
                                           topic_Client_Account),
                                 word = c(CHF_Loans,
                                          Anatocism,
                                          Personal_Loan_Reimbursement, 
                                          Derivatives_Misselling,
                                          Client_Account))
    }
    if(ET.Filter[i] == "EL0500 - Damage to Physical Assets"){
      ATM_Damage = c("atm",
                 "vandalism_bgn_pay",
                 "break_vandalism_bgn",
                 "damage_bgn_pay",
                 "keypad_break_vandalism",
                 "bgn_pay_repair",
                 "claim_insurer_insurance",
                 "insurer_insurance_payment",
                 "repair_claim_insurer",
                 "insurance_payment_expect",
                 "payment_expect_insurance",
                 "expect_insurance_payment",
                 "insurance_payment_bgn")
      topic_ATM_Damage <- as.factor(rep("EL0501_ATM_Damage", length(ATM_Damage)))
      Car_Damage = c("car_damage",
                     "car_accident",
                     "vehicle", 
                     "damage_party_error",
                     "casco",
                     "car_park"
                     )
      topic_Car_Damage <- as.factor(rep("EL0502_Car_Damage", length(Car_Damage)))
      seeded_words <- data.frame(topic = c(topic_ATM_Damage, topic_Car_Damage),
                                 word = c(ATM_Damage, Car_Damage))
    }
    if(ET.Filter[i] == "EL0600 - Business Disruption and System Failures"){
      Software_Bugs = c(
                        "bug", 
                        "software")
      topic_Software_Bugs <- as.factor(rep("EL0601_Software_Bugs", length(Software_Bugs)))
      Loss_IT_Problem = c("loss_problem")
      topic_Loss_IT_Problem <- as.factor(rep("EL0602_Loss_IT_Problem", length(Loss_IT_Problem)))
      seeded_words <- data.frame(topic = c(topic_Software_Bugs, topic_Loss_IT_Problem),
                                 word = c(Software_Bugs, Loss_IT_Problem))
    }
    if(ET.Filter[i] == "EL0700 - Execution, Delivery & Process Management"){
      Inadequate_Data = c("inadequate_data", 
                          "record", 
                          "address", 
                          "statement",
                          "bank_cause_expense",
                          "client_return_bank"
                          )
      topic_Inadequate_Data <- as.factor(rep("EL0701_Inadequate_Data", length(Inadequate_Data)))
      Cash_Differences = c("cash", 
                           "reconcile", 
                           "different" 
                           )
      topic_Cash_Differences <- as.factor(rep("EL0702_Cash_Differences", length(Cash_Differences)))
      Client_Account = c(
                         "account" 
                         )
      topic_Client_Account <- as.factor(rep("EL0703_Client_Account", length(Client_Account)))
      Error_False_Notification = c("error_false_notification")
      topic_Error_False_Notification <- as.factor(rep("EL0704_Error_False_Notification", 
                                                      length(Error_False_Notification)))
      seeded_words <- data.frame(topic = c(topic_Inadequate_Data, topic_Cash_Differences,
                                           topic_Client_Account, topic_Error_False_Notification),
                                 word = c(Inadequate_Data, Cash_Differences, Client_Account,
                                          Error_False_Notification))
    }
    
    seeded_words_df <- rbind(seeded_words_df, seeded_words)

  }
  
  date <- Sys.time()
  date <- as.character(date)
  date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
  save(seeded_words_df, file = file.path(path ,paste0("seeded_words_", date, ".RData")))
  
  # Dictionary for seeded words
  toks_seeded <- seeded_words_df$word
  dict_seeded_list <- as.list(paste0("*", toks_seeded, "*"))
  names(dict_seeded_list) <- toks_seeded 
  dict_seeded <- dictionary(dict_seeded_list)
  
  # Integrate seeded words dictionary into the tokens
  toks_Corep <- tokens_lookup(toks_Corep, dictionary = dict_seeded, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)
  toks <- tokens_lookup(toks, dictionary = dict_seeded, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)
  

}


print("Creation of the document-by-term matrix")

tic()

# dfm definition ####

# Minimum frequency for tokens
MinTermFreq <- 5


Description_dfm <- toks %>% 
  dfm() %>%
  dfm_trim(max_docfreq = NULL, min_termfreq = MinTermFreq, termfreq_type = "count") 
Description_dfm_Corep <- toks_Corep %>% 
  dfm() %>%
  dfm_trim(max_docfreq = NULL, min_termfreq = MinTermFreq, termfreq_type = "count")

# dimensions of dfm
dim(Description_dfm)
dim(Description_dfm_Corep)

toc()


# Use the `topfeatures()` function to inspect the top 100 most frequently occurring features in the dfm
topfeatures(Description_dfm, 100)
topfeatures(Description_dfm_Corep, 100)

myDf <- data.frame(Description_dfm@Dimnames[["features"]])
myDf_Corep <- data.frame(Description_dfm_Corep@Dimnames[["features"]])
# write.table(x = myDf, file = file.path(path,"myDf20230227.csv"), quote = F, sep = ";", row.names = F, col.names = T)

# Wordcloud
textplot_wordcloud(Description_dfm, min_count = 20,adjust = 0.5, max_words = 150, rotation = 0, fixed_aspect = T, 
                   color = c('gray48','black','lightcoral','turquoise4', 'lawngreen', 'orange', 'green3', 'darkorchid', 'forestgreen', 'mediumblue', 'red1'))
textplot_wordcloud(Description_dfm_Corep, min_count = 20,adjust = 0.5, max_words = 150, rotation = 0, fixed_aspect = T, 
                   color = c('gray48','black','lightcoral','turquoise4', 'lawngreen', 'orange', 'green3', 'darkorchid', 'forestgreen', 'mediumblue', 'red1'))


# Training of word embedding ####

RecalcWordEmbIntegration <- TRUE # FALSE # 

if(RecalcWordEmbIntegration){

print("Training of word embedding")
tic()
  
# Convert tokens to character object
InternalDescription <- sapply(toks, paste, collapse = " ")

# Read External data for w2v
ExternalOpData <- fread(file = file.path(path,"ORX_NEWS_20230131.csv"), header=TRUE, sep = ";", check.names = FALSE,blank.lines.skip = T)
ExternalOpData <- data.frame(ExternalOpData)
dim(ExternalOpData)
# Rename the description column
colnames(ExternalOpData)[colnames(ExternalOpData)=='Digest.Text'] <- "ExternalDescription"

# If a string is declared to be neither in ASCII nor in UTF-8, 
# then all byte codes > 127 are replaced with the Unicode REPLACEMENT CHARACTER (\Ufffd).
ExternalOpData$ExternalDescription <- stri_enc_toutf8(ExternalOpData$ExternalDescription, is_unknown_8bit = TRUE)
# Remove whitespace from the end of each description
ExternalOpData$ExternalDescription <- str_trim(ExternalOpData$ExternalDescription,side = c("both", "left", "right"))
ExternalDescription <- ExternalOpData$ExternalDescription
# Remove the Unicode REPLACEMENT CHARACTER (\Ufffd)
ExternalDescription <- ExternalDescription %>%
  str_replace_all("\Ufffd", "")

# Force lower case
ExternalDescription <- tolower(ExternalDescription)

# Corpus structure
ExternalDescription_corpus <- corpus(ExternalDescription)

# Assign Event IDs as description names
names(ExternalDescription_corpus) <- ExternalOpData$Story.Reference.Number

# Tokenizing text and creation of the Document-feature matrix (dfm)
toks_External <- tokens(ExternalDescription_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T, remove_url = T)
typesnum_External <- grep("[[:digit:]]", types(toks_External), value = TRUE) # replace the digit number from 0 to 9 with white space
toks_External <- tokens_replace(toks_External, typesnum_External, gsub("[[:digit:]]", "\\s+", typesnum_External))
typeshyphens_end_External <- grep(".-$", types(toks_External), value = TRUE)
toks_External <- tokens_replace(toks_External, typeshyphens_end_External, gsub(".-$", "\\s+", typeshyphens_end_External))
strange_character_External <- grep("\\+s+", types(toks_External), value = TRUE)
toks_External <- tokens_replace(toks_External, strange_character_External, gsub("\\+s+", "\\s", strange_character_External))
# https://search.r-project.org/CRAN/refmans/lexicon/html/hash_lemmas.html
toks_External <- tokens_replace(toks_External, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
typeplus_External <- grep("[+]", types(toks_External), value = TRUE)
typeTwoLetters_External <- substr(types(toks_External), start = 1, stop = 2)

# Integrate dictionary into the tokens
toks_External <- tokens_lookup(toks_External, dictionary = dict, exclusive = FALSE, capkeys = FALSE, case_insensitive=T)

# Remove stopwords from tokens (useful for n-grams selection analysis)
toks_External <- toks_External %>%
  tokens_remove(pattern = c(stopwords("en"), OtherStopwords, typeshyphens_end_External, typeplus_External, typeTwoLetters_External)) %>%
  tokens_remove(pattern = stopwords("it")) %>%
  tokens_remove(pattern = stopwords("de"))

# Count of tokens (total features)
sum(ntoken(toks_External))
# Count of types (unique tokens)
sum(ntype(toks_External))

# Integrate 3-gram dictionary into the tokens
toks_External <- tokens_lookup(toks_External, dictionary = dict_3gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate 2-gram dictionary into the tokens
toks_External <- tokens_lookup(toks_External, dictionary = dict_2gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate other dictionary into the tokens
toks_External <- tokens_lookup(toks_External, dictionary = dict_other, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate seeded words dictionary into the tokens
if(seeded_words_dictionary){
  toks_External <- tokens_lookup(toks_External, dictionary = dict_seeded, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)
}

# Convert tokens to character object
ExternalDescription <- sapply(toks_External, paste, collapse = " ")

# Remove fixed part of the description
ExternalDescription <- ExternalDescription %>%
  str_replace_all("datum supply orx news operational riskdata exchange association orx", "")
ExternalDescription <- ExternalDescription %>%
  str_replace_all("data supply orx news operational riskdata exchange association orx", "")

# Remove empty descriptions
ExternalDescription <- ExternalDescription[!(ExternalDescription=="")]


# Read Scenario data for w2v
ScenarioOpData <- fread(file = file.path(path,"ScenarioData_2022Q4_20230224.csv"), header=TRUE, sep = ";", check.names = FALSE, blank.lines.skip = T)
ScenarioOpData <- data.frame(ScenarioOpData)
dim(ScenarioOpData)
# Rename the description column
names(ScenarioOpData)[12] <- "ScenarioDescription"
# then all byte codes > 127 are replaced with the Unicode REPLACEMENT CHARACTER (\Ufffd).
ScenarioOpData$ScenarioDescription <- stri_enc_toutf8(ScenarioOpData$ScenarioDescription, is_unknown_8bit = TRUE)
# Remove whitespace from the end of each description
ScenarioOpData$ScenarioDescription <- str_trim(ScenarioOpData$ScenarioDescription,side = c("both", "left", "right"))
ScenarioDescription <- ScenarioOpData$ScenarioDescription
# Remove the Unicode REPLACEMENT CHARACTER (\Ufffd)
ScenarioDescription <- ScenarioDescription %>%
  str_replace_all("\Ufffd", "")
# Force lower case
ScenarioDescription <- tolower(ScenarioDescription)

# Corpus structure
ScenarioDescription_corpus <- corpus(ScenarioDescription)

# Assign Event IDs as description names
names(ScenarioDescription_corpus) <- ScenarioOpData$ID_SCENARIO_1IN10Y

# Tokenizing text
toks_Scenario <- tokens(ScenarioDescription_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T, remove_url = T)
typesnum_Scenario <- grep("[[:digit:]]", types(toks_Scenario), value = TRUE) # replace the digit number from 0 to 9 with white space
toks_Scenario <- tokens_replace(toks_Scenario, typesnum_Scenario, gsub("[[:digit:]]", "\\s+", typesnum_Scenario))
typeshyphens_end_Scenario <- grep(".-$", types(toks_Scenario), value = TRUE)
toks_Scenario <- tokens_replace(toks_Scenario, typeshyphens_end_Scenario, gsub(".-$", "\\s+", typeshyphens_end_Scenario))
strange_character_Scenario <- grep("\\+s+", types(toks_Scenario), value = TRUE)
toks_Scenario <- tokens_replace(toks_Scenario, strange_character_Scenario, gsub("\\+s+", "\\s", strange_character_Scenario))
# https://search.r-project.org/CRAN/refmans/lexicon/html/hash_lemmas.html
toks_Scenario <- tokens_replace(toks_Scenario, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
typeplus_Scenario <- grep("[+]", types(toks_Scenario), value = TRUE)
typeTwoLetters_Scenario <- substr(types(toks_Scenario), start = 1, stop = 2)

# Integrate dictionary into the tokens
toks_Scenario <- tokens_lookup(toks_Scenario, dictionary = dict, exclusive = FALSE, capkeys = FALSE, case_insensitive=T)

# Remove stopwords from tokens (useful for n-grams selection analysis)
toks_Scenario <- toks_Scenario %>%
  tokens_remove(pattern = c(stopwords("en"), OtherStopwords, typeshyphens_end_Scenario, typeplus_Scenario, typeTwoLetters_Scenario)) %>%
  tokens_remove(pattern = stopwords("it")) %>%
  tokens_remove(pattern = stopwords("de"))

# Count of tokens (total features)
sum(ntoken(toks_Scenario))
# Count of types (unique tokens)
sum(ntype(toks_Scenario))

# Integrate 3-gram dictionary into the tokens
toks_Scenario <- tokens_lookup(toks_Scenario, dictionary = dict_3gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate 2-gram dictionary into the tokens
toks_Scenario <- tokens_lookup(toks_Scenario, dictionary = dict_2gram, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate other dictionary into the tokens
toks_Scenario <- tokens_lookup(toks_Scenario, dictionary = dict_other, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)

# Integrate seeded words dictionary into the tokens
if(seeded_words_dictionary){
  toks_Scenario <- tokens_lookup(toks_Scenario, dictionary = dict_seeded, exclusive = FALSE, capkeys = FALSE, case_insensitive = TRUE)
}

# Convert tokens to character object
ScenarioDescription <- sapply(toks_Scenario, paste, collapse = " ")

# Remove empty descriptions
ScenarioDescription <- ScenarioDescription[!(ScenarioDescription=="")]



# Aggregate descriptions 
rm(Description)
gc()
AllDescription <- c(InternalDescription, ExternalDescription, ScenarioDescription)

set.seed(123456789)

split_w2v = c(" \n,.-!?:;/\"#$%&'()*+<=>@[]\\^`{|}~\t\v\f\r", ".\n?!") # default of wor2vec excluding "_" to avoid splitting n-grams

# w2v on all data (internal,external, and scenario) - cbow
All_cbow <- word2vec(x = AllDescription, type = "cbow", dim = 100, split = split_w2v) 

# w2v on all data (internal,external, and scenario) skip-gram
All_skipgram <- word2vec(x = AllDescription, type = "skip-gram", dim = 100, split = split_w2v)

# Check the 5 nearest words for relevant terms 
predict(All_cbow, newdata = c("bank"), type = "nearest", top_n = 5) 
predict(All_cbow, newdata = c("client"), type = "nearest", top_n = 5) 
predict(All_cbow, newdata = c("anatocism"), type = "nearest", top_n = 5)  
predict(All_cbow, newdata = c("legal"), type = "nearest", top_n = 5) 
predict(All_skipgram, newdata = c("bank"), type = "nearest", top_n = 5) 
predict(All_skipgram, newdata = c("client"), type = "nearest", top_n = 5)
predict(All_skipgram, newdata = c("anatocism"), type = "nearest", top_n = 5)
predict(All_skipgram, newdata = c("legal"), type = "nearest", top_n = 5)

toc()

print("Perform semantic adjustment")

tic()

gc()
embFilterAll <- as.matrix(All_cbow)
length(rownames(embFilterAll))


# Select words in our dictionaries
embFilterAll <- embFilterAll[which(rownames(embFilterAll) %in% myDf_Corep[,1]),]
dim(embFilterAll)


# Compute word similarity matrix
embFilter_distanceAll <- textstat_simil(as.dfm(embFilterAll), margin = "documents", method = "cosine", min_simil = 0)
embFilter_distanceAll <- as.data.frame(embFilter_distanceAll)
embFilter_distanceAll <- embFilter_distanceAll[embFilter_distanceAll$document1 != embFilter_distanceAll$document2,]
dim(embFilter_distanceAll) # 2698483      3
head(embFilter_distanceAll)


gc()

dim(embFilter_distanceAll)


date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(embFilter_distanceAll, file = file.path(path, paste0("embFilter_distanceAll_", date, ".RData")))



# Apply semantic adjustment ####

thrSim <- 0.8
embFilter_distanceAll <- embFilter_distanceAll[embFilter_distanceAll$cosine > thrSim,]
dim(embFilter_distanceAll)
head(embFilter_distanceAll)


  Semantic_tfidf <- Description_dfm_Corep # TF
  
  dim(Semantic_tfidf)
  
  rm(Description_dfm)
  rm(Description_dfm_Corep)
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
  ind_emb <- rep(0, n_rows)
  for(i in 1:n_rows){
    #print(i)
    if(sum((toks_Corep[[i]] %fin% embFilter_distanceAll$document1) & (toks_Corep[[i]] %fin% embFilter_distanceAll$document2)) > 0){
      ind_emb[i] <- 1
    }
  }
  #toc()
  
  ind_emb <- which(ind_emb == 1)
  
  
  #tic()
  for(i in ind_emb){
    #print(i)
    subSemantic_tfidf <- Semantic_tfidf[i,][,which(Semantic_tfidf[i,]>0)]
    names_subSemantic_tfidf <- names(subSemantic_tfidf)
    # filter here embFilter_distance with the words in document i
    subEmbFilter_distance <- embFilter_distanceAll[(embFilter_distanceAll$document1 %fin% names_subSemantic_tfidf) |
                                                  (embFilter_distanceAll$document2 %fin% names_subSemantic_tfidf),]
    if(nrow(subEmbFilter_distance)>0){
      #tic()
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
  

  date <- Sys.time()
  date <- as.character(date)
  date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
  save(Semantic_tfidf, file = file.path(path, paste0("Semantic_tfidf_thr", thrSim*100, "_", date, ".RData")))
  
  toc()

}else{

  load(file.path(path,"Semantic_tfidf_thr80_2023-11-14_16-37.RData"))
}
  
gc()


# Cleaned descriptions ####

print("Include cleaned descriptions (based on tokenization) and select non-zero rows")

tic()

# Convert Corep tokens to character object
CleanedDescription <- sapply(toks_Corep, paste, collapse = " \u000A\ ")

# Add cleaned descriptions into Corep data
OpData_Corep$Description_Cleaned <- CleanedDescription

OpData.Filter <- OpData_Corep 
dim(OpData.Filter)


# Select data above a threshold (otherwise leave -Inf)
thr <- -Inf
OpData.Filter <- OpData.Filter[which(OpData.Filter$`Gross Loss` >= thr),]
dim(OpData.Filter)


dim(Semantic_tfidf)
Semantic_tfidf.Filter <- Semantic_tfidf[row.names(Semantic_tfidf) %in% OpData.Filter$ID,]
dim(Semantic_tfidf.Filter)

gc()

# Select non-zero rows
sums <- ntoken(Semantic_tfidf.Filter)
Semantic_tfidf.Filter <- dfm_subset(Semantic_tfidf.Filter, sums > 0)
OpData.Filter <- OpData.Filter[sums != 0,]
dim(Semantic_tfidf.Filter)
dim(OpData.Filter)
# Normalize vectors
Normalize <- FALSE
if(Normalize){
  for (i in 1:nrow(Semantic_tfidf.Filter)) {
    Semantic_tfidf.Filter[i,] <- Semantic_tfidf.Filter[i,] / sqrt(sum(Semantic_tfidf.Filter[i,]^2))  
  }
}


rm(toks_2gram)
rm(toks_2gram_dfm)
rm(toks_3gram)
rm(toks_3gram_dfm)


gc()
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_tfidf.Filter, file = file.path(path ,paste0("Semantic_tfidf.Filter_", date, ".RData")))
gc()

toc()



# PCA #### 
# https://stats.stackexchange.com/questions/35185/dimensionality-reduction-svd-or-pca-on-a-large-sparse-matrix

print("PCA of Group")
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
save(Semantic_tfidf.Filter.pca, file = file.path(path, paste0("Semantic_tfidf.Filter.pca_", date, ".RData")))
gc()

summary(Semantic_tfidf.Filter.pca)

fviz_eig(Semantic_tfidf.Filter.pca, ncp = pca.dim, addlabels = TRUE, main = "Scree plot of Group")
# Graph of the variables
fviz_pca_var(Semantic_tfidf.Filter.pca, col.var = "black")

Semantic_tfidf.Filter.Matrix <- Semantic_tfidf.Filter.pca$x
Semantic_tfidf.Filter.Matrix.Loadings <- Semantic_tfidf.Filter.pca$rotation


Semantic_pca.Filter.docs1 <- Semantic_tfidf.Filter.Matrix[, 1]
Semantic_pca.Filter.docs2 <- Semantic_tfidf.Filter.Matrix[, 2]
Semantic_pca.Filter.features1 <- Semantic_tfidf.Filter.Matrix.Loadings[, 1]
Semantic_pca.Filter.features2 <- Semantic_tfidf.Filter.Matrix.Loadings[, 2]
rownames.Semantic_pca.Filter.docs <- rownames(Semantic_tfidf.Filter)
rownames.Semantic_pca.Filter.features <- colnames(Semantic_tfidf.Filter)

df_pca <- data.frame(x=Semantic_pca.Filter.docs1, y=Semantic_pca.Filter.docs2,
                     id_desc=paste(rownames.Semantic_pca.Filter.docs, OpData.Filter$Description_v3,
                                   "\u000A\ CLEANED:", OpData.Filter$Description_Cleaned))

df_pca_features <- data.frame(x=Semantic_pca.Filter.features1, y=Semantic_pca.Filter.features2,
                              id_features=paste(rownames.Semantic_pca.Filter.features))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_pca, file = file.path(path, paste0("df_pca_NoCluster_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_pca_features, file = file.path(path, paste0("df_pca_features_", date, ".RData")))

plot_ly(df_pca, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
  layout(title = "PCA of Group")

plot_ly(df_pca_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
  layout(title = "PCA terms of Group")

toc()

# LSA ####

print("LSA of Group")
tic()

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
                     id_desc=paste(rownames.Semantic_lsa.Filter.docs, OpData.Filter$Description_v3,
                                   "\u000A\ CLEANED:", OpData.Filter$Description_Cleaned))

df_lsa_features <- data.frame(x=Semantic_lsa.Filter.features1, y=Semantic_lsa.Filter.features2,
                              id_features=paste(rownames.Semantic_lsa.Filter.features))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_lsa, file = file.path(path, paste0("df_lsa_NoCluster_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_lsa_features, file = file.path(path, paste0("df_lsa_features_", date, ".RData")))

plot_ly(df_lsa, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
  layout(title = paste0("LSA of Group"))

plot_ly(df_lsa_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
  layout(title = paste0("LSA terms of Group"))

gc()

toc()


# UMAP ####

print("UMAP of Group")

tic()

#tic()
Semantic_tfidf.Filter.umap = uwot::umap(Semantic_tfidf.Filter.Matrix, 
                                        n_neighbors = 15, # 2, # 
                                        n_components = 2,
                                        n_epochs = NULL, 
                                        #pca = pca.dim, # Not used, since it already starts from PCA result
                                        init = "spectral")
#toc()

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_tfidf.Filter.umap, file = file.path(path, paste0("Semantic_tfidf.Filter.umap_", date, ".RData")))

layout <- Semantic_tfidf.Filter.umap
layout <- data.frame(layout)
df_umap <- cbind(layout, 
                 id_desc=paste(OpData.Filter$ID, OpData.Filter$Description_v3,
                               "\u000A\ CLEANED:", OpData.Filter$Description_Cleaned)
                )

df_umap <- as.data.frame(df_umap)

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_umap, file = file.path(path, paste0("df_umap_NoCluster_", date, ".RData")))

plot_umap <- plot_ly(df_umap, x = ~X1, y = ~X2, 
                     text = ~id_desc, 
                     type = 'scatter', mode = 'markers') %>%
  layout(
    plot_bgcolor = "#e5ecf6",
    xaxis = list(
      title = "V1"),
    yaxis = list(
      title = "V2"),
    title = "UMAP of Group")
plot_umap

gc()
rm(Semantic_tfidf.Filter.Matrix)
gc()

toc()



# Plot by ET ####

print("PCA, LSA and UMAP by Event Type")

ET <- sort(unique(OpData.Filter$'Event Type'))

Semantic_pca_ET <- list()
Semantic_lsa.Filter.ET <- list()
Semantic_tfidf.Filter.umap.ET <- list()
df_pca_ET <- list()
df_pca_features_ET <- list()
df_lsa_ET <- list()
df_lsa_features_ET <- list()
df_umap_ET <- list()
l_pca <- list()
l_pca_terms <- list()
l_pca_scree <- list()
l_lsa <- list()
l_lsa_terms <- list()
l_umap <- list()

for(i in 1:length(ET)){
  print(paste0("Event type ", ET[i]))
  OpData_ET <- OpData.Filter[OpData.Filter$'Event Type' == ET[i],]
  Semantic_tfidf_ET <- Semantic_tfidf.Filter[row.names(Semantic_tfidf.Filter) %in% OpData_ET$ID,]
  
  # PCA
  print(paste0("PCA of ", ET[i]))
  tic()
  pca.dim <- min(c(50, dim(Semantic_tfidf_ET)))
  Semantic_pca_ET[[i]] <- prcomp_irlba(Semantic_tfidf_ET, n=pca.dim)

  print(paste0("Summary PCA of ", ET[i]))
  print(summary(Semantic_pca_ET[[i]]))
  l_pca_scree[[i]] <- fviz_eig(Semantic_pca_ET[[i]], ncp = pca.dim, addlabels = TRUE, main = paste0("Scree plot of ", ET[i]))
  
  Semantic_tfidf.Filter.Matrix <- Semantic_pca_ET[[i]]$x
  Semantic_tfidf.Filter.Matrix.Loadings <- Semantic_pca_ET[[i]]$rotation
  
  Semantic_pca.Filter.docs1 <- Semantic_tfidf.Filter.Matrix[, 1]
  Semantic_pca.Filter.docs2 <- Semantic_tfidf.Filter.Matrix[, 2]
  Semantic_pca.Filter.features1 <- Semantic_tfidf.Filter.Matrix.Loadings[, 1]
  Semantic_pca.Filter.features2 <- Semantic_tfidf.Filter.Matrix.Loadings[, 2]
  rownames.Semantic_pca.Filter.docs <- rownames(Semantic_tfidf_ET)
  rownames.Semantic_pca.Filter.features <- colnames(Semantic_tfidf_ET)
  
  df_pca_ET[[i]] <- data.frame(x=Semantic_pca.Filter.docs1, y=Semantic_pca.Filter.docs2,
                       id_desc=paste(rownames.Semantic_pca.Filter.docs, OpData_ET$Description_v3,
                                     "\u000A\ CLEANED:", OpData_ET$Description_Cleaned))
  
  df_pca_features_ET[[i]] <- data.frame(x=Semantic_pca.Filter.features1, y=Semantic_pca.Filter.features2,
                                id_features=paste(rownames.Semantic_pca.Filter.features))
  
  l_pca[[i]] <- plot_ly(df_pca_ET[[i]], x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
    layout(title = paste0("PCA of ", ET[i]))
  
  l_pca_terms[[i]] <- plot_ly(df_pca_features_ET[[i]], x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
    layout(title = paste0("PCA terms of ", ET[i]))
  
  toc()
  
  # LSA
  print(paste0("LSA of ", ET[i]))
  tic()
  Semantic_lsa.Filter.ET[[i]] <- textmodel_lsa(as.dfm(Semantic_tfidf.Filter.Matrix), nd = 2)
  
  Semantic_lsa.Filter.docs1 <- Semantic_lsa.Filter.ET[[i]]$docs[, 1]
  Semantic_lsa.Filter.docs2 <- Semantic_lsa.Filter.ET[[i]]$docs[, 2]
  Semantic_lsa.Filter.features1 <- Semantic_lsa.Filter.ET[[i]]$features[, 1]
  Semantic_lsa.Filter.features2 <- Semantic_lsa.Filter.ET[[i]]$features[, 2]
  rownames.Semantic_lsa.Filter.docs <- rownames(Semantic_lsa.Filter.ET[[i]]$docs)
  rownames.Semantic_lsa.Filter.features <- rownames(Semantic_lsa.Filter.ET[[i]]$features)


  df_lsa_ET[[i]] <- data.frame(x=Semantic_lsa.Filter.docs1, y=Semantic_lsa.Filter.docs2,
                       id_desc=paste(rownames.Semantic_lsa.Filter.docs, OpData_ET$Description_v3,
                                     "\u000A\ CLEANED:", OpData_ET$Description_Cleaned))
  
  df_lsa_features_ET[[i]] <- data.frame(x=Semantic_lsa.Filter.features1, y=Semantic_lsa.Filter.features2,
                                id_features=paste(rownames.Semantic_lsa.Filter.features))
  
  l_lsa[[i]] <- plot_ly(df_lsa_ET[[i]], x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
    layout(title = paste0("LSA of ", ET[i]))
  
  l_lsa_terms[[i]] <- plot_ly(df_lsa_features_ET[[i]], x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
    layout(title = paste0("LSA terms of ", ET[i]))
  
  toc()
  gc()
  

  # UMAP
  print(paste0("UMAP of ", ET[i]))
  tic()
  Semantic_tfidf.Filter.umap.ET[[i]] <- uwot::umap(Semantic_tfidf.Filter.Matrix, 
                                          n_neighbors = 15,  
                                          n_components = 2,
                                          n_epochs = NULL, 
                                          #pca = pca.dim, # Not used, since it already starts from PCA result
                                          init = "spectral")
  
  layout <- Semantic_tfidf.Filter.umap.ET[[i]]
  layout <- data.frame(layout)
  df_umap_ET[[i]] <- cbind(layout, 
                   id_desc=paste(OpData_ET$ID, OpData_ET$Description_v3,
                                 "\u000A\ CLEANED:", OpData_ET$Description_Cleaned)
  )
  
  df_umap_ET[[i]] <- as.data.frame(df_umap_ET[[i]])
  
  l_umap[[i]] <- plot_ly(df_umap_ET[[i]], x = ~X1, y = ~X2, 
                       text = ~id_desc, 
                       type = 'scatter', mode = 'markers') %>%
    layout(
      plot_bgcolor = "#e5ecf6",
      xaxis = list(
        title = "V1"),
      yaxis = list(
        title = "V2"),
      title = paste0("UMAP of ", ET[i]))
  
  toc()
  
  gc()
  rm(Semantic_tfidf.Filter.Matrix)
  gc()
  
}

l_pca
l_pca_terms
l_pca_scree
l_lsa
l_lsa_terms
l_umap


print("Save data to reproduce PCA, LSA and UMAP by Event Type")
tic()

gc()
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_pca_ET, file = file.path(path, paste0("Semantic_pca_ET_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_lsa.Filter.ET, file = file.path(path, paste0("Semantic_lsa.Filter.ET_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_pca_ET, file = file.path(path, paste0("df_pca_NoCluster_ET_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_pca_features_ET, file = file.path(path, paste0("df_pca_features_ET_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_lsa_ET, file = file.path(path, paste0("df_lsa_NoCluster_ET_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_lsa_features_ET, file = file.path(path, paste0("df_lsa_features_ET_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_umap_ET, file = file.path(path, paste0("df_umap_NoCluster_ET_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_tfidf.Filter.umap.ET, file = file.path(path, paste0("Semantic_tfidf.Filter.umap_ET_", date, ".RData")))
gc()

toc()


# UMAP by ET - Sensitivity ####

umap.sensitivity <- FALSE

if(umap.sensitivity){
  
  print("Sensitivity of UMAP by ET")
  tic()

  n_neighbors_umap_vec <- c(5, 10, 15, 20, 50, 100)
  min_dist_umap_vec <- c(0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 1)
  
  for(k in 1:length(n_neighbors_umap_vec)){
    print(k)
    for(j in 1:length(min_dist_umap_vec)){
      print(j)
  
      l <- list()
      Semantic_tfidf.Filter.umap.ET <- list()
      for(i in 1:length(ET)){
        print(i)
        set.seed(123)
        OpData_ET <- OpData.Filter[OpData.Filter$'Event Type' == ET[i],]
        Semantic_tfidf_ET <- Semantic_tfidf.Filter[row.names(Semantic_tfidf.Filter) %in% OpData_ET$ID,]
        Semantic_tfidf.Filter.Matrix <- as.matrix(Semantic_tfidf_ET)
        dim(Semantic_tfidf.Filter.Matrix)
        gc()
        pca.dim <- min(c(50, dim(Semantic_tfidf.Filter.Matrix)))
        Semantic_tfidf.Filter.umap.ET[[i]] = uwot::umap(Semantic_tfidf.Filter.Matrix,
                                                n_neighbors = n_neighbors_umap_vec[k],
                                                min_dist = min_dist_umap_vec[j],
                                                n_components = 2,
                                                n_epochs = NULL,
                                                pca = pca.dim,
                                                init = "spectral")
        
        layout <- Semantic_tfidf.Filter.umap.ET[[i]]
        layout <- data.frame(layout)
        df_umap <- cbind(layout, 
                         id_desc=paste(OpData_ET$ID, OpData_ET$Description_v3,
                                       "\u000A\ CLEANED:", OpData_ET$Description_Cleaned)
      
        )
        
        df_umap <- as.data.frame(df_umap)
        
        l[[i]] <- plot_ly(df_umap, x = ~X1, y = ~X2, 
                             text = ~id_desc, 
                             type = 'scatter', mode = 'markers') %>%
          layout(
            plot_bgcolor = "#e5ecf6",
            xaxis = list(
              title = "V1"),
            yaxis = list(
              title = "V2"),
            title = paste0("Event Type ", ET[i]))
      }
      l

      
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      save(Semantic_tfidf.Filter.umap.ET,
           file = file.path(path,
                            paste0("Semantic_tfidf.Filter.umap.ET_", 
                                   n_neighbors_umap_vec[k], "_",
                                   min_dist_umap_vec[j],
                                   "_", date, ".Rdata")))
  
  
  
    }
  }
  
  
  load("W:/TextAnalysis/data/Semantic_tfidf.Filter.umap.ET_15_0.01_20230925.Rdata")
  
  
  for(i in 1:length(ET)){
    print(i)
    set.seed(123)
    OpData_ET <- OpData.Filter[OpData.Filter$'Event Type' == ET[i],]

    layout <- Semantic_tfidf.Filter.umap.ET[[i]]
    layout <- data.frame(layout)
    df_umap <- cbind(layout, 
                     id_desc=paste(OpData_ET$ID, OpData_ET$Description_v3,
                                   "\u000A\ CLEANED:", OpData_ET$Description_Cleaned)
                     
    )
    
    df_umap <- as.data.frame(df_umap)
    
    l[[i]] <- plot_ly(df_umap, x = ~X1, y = ~X2, 
                      text = ~id_desc, 
                      type = 'scatter', mode = 'markers') %>%
      layout(
        plot_bgcolor = "#e5ecf6",
        xaxis = list(
          title = "V1"),
        yaxis = list(
          title = "V2"),
        title = paste0("Event Type ", ET[i]))
  }
  l
  
  
  gc()

  date <- Sys.time()
  date <- as.character(date)
  date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
  save.image(file = file.path(path, paste0("ResultsBeforeLda", "_", date, ".Rdata")))
  gc()
  
  toc()

}



# Latent Dirichlet Allocation (LDA) ####

# For LDA
library(textmineR)
# For wordclouds and seeded LDA
library(topicmodels)
library(wordcloud)


LDA_ET_Seeded_topicmodels <- TRUE

if(LDA_ET_Seeded_topicmodels){
  
print("Seeded LDA by Event Type")

# Seeded LDA by ET using topicmodels #### 

ET <- sort(unique(OpData.Filter$'Event Type'))

# Define the Cluster column
OpData.Filter$Cluster <- OpData.Filter$'Event Type'

# Select the ETs to be split
ET.Filter <- ET[c(1, 2, 3, 4, 5, 6, 7)]

# Number of topics for each ET.Filter
if(seeded_words_dictionary){
  nTopics.ET.Filter <- c(4, 4, 4, 6, 3, 4, 5)
}else{
  nTopics.ET.Filter <- c(4, 4, 4, 7, 3, 4, 5)
}


residualTopic.ET.Filter <- nTopics.ET.Filter


# Topic Modeling settings
SeedWeight <- 10^300 

num.iterations_par <- c(10^3, 10^3, 10^3, 10^3, 10^3, 10^3, 10^3)
burn_par <- num.iterations_par / 2 

alpha_par <- 50 / nTopics.ET.Filter
seed <- 1L

# Seed for TF matrix (it cannot be too high to be recognized as integer value)
Seeded_tf <- FALSE
SeedWeight_tf <- 10

# Weight of non-seeded words (default is 0.1)
deltaW <- 0.1 

# Plot wordclouds
wordcloud_plot <- TRUE # FALSE #


# Initialize the complete data frame of seeded words
seeded_words_complete <- data.frame(topic = NULL,
                           word = NULL)

## create a dataframe to store the perplexity scores for different values of k
perp.ET <- data.frame(ET = ET.Filter, Perplexity = NA)
perp.calc <- TRUE # FALSE #
trainRatio <- 0.9 # 0.75 #

# Recalculate the ProbTopics as the the average of topic probabilities
# related to identical documents
Averaged_LDA <- TRUE # FALSE #
Averaged_LDA_Loaded <- FALSE

# Apply constraint on seeds for topic assignment
Constrainted <- TRUE # FALSE # 

model <- list()
ProbTopics <- list()
ProbTopicsNoAvg <- list()
ProbWords <- list()

for(i in 1:length(ET.Filter)){
  
  print(paste0("Seeded LDA of ", ET.Filter[i]))
  
  print(paste0("Select data and seeds of ", ET.Filter[i]))
  tic()
  
  OpData_ET <- OpData.Filter[OpData.Filter$'Event Type' == ET.Filter[i], ]
  Semantic_tfidf_ET <- Semantic_tfidf.Filter[row.names(Semantic_tfidf.Filter) %in% OpData_ET$ID, ]
  
  k <- nTopics.ET.Filter[i]
  
  set.seed(12345)
  
  if(!seeded_words_dictionary){
  
    if(ET.Filter[i] == "EL0100 - Internal Fraud"){
      Client_Account <- c("client_account",
                          "money_client_account",
                          "account_payment",
                          "payment_account",
                          "client_bank",
                          "unauthorized",
                          "unauthorized_transaction_client",
                          "fraud_transaction_client_account",
                          "unauthorized_transaction")
      topic_Client_Account <- as.factor(rep("EL0101_Client_Account", length(Client_Account)))
      Unfaithfulness <- c("Unfaithfulness",
                          "branch",
                          "branch_employee",
                          "Unfaithful_employee",
                          "employee_Unfaithfulness",
                          "infedelta",
                          "embezzlement",
                          "employee",
                          "ucb_employee",
                          "merchant",
                          "finance_agent",
                          "fineco",
                          "bank",
                          "terni",
                          "latina",
                          "bipop",
                          "manerbio",
                          "roma",
                          "aversa",
                          "benevento",
                          "firenze")
      topic_Unfaithfulness <- as.factor(rep("EL0102_Unfaithfulness", length(Unfaithfulness)))
      ATM_Fraud <- c("atm",
                     "cash_box",
                     "supply",
                     "empty",
                     "cash_operation",
                     "refill",
                     "banknote",
                     "shortage_cash_register",
                     "cash_register_employee",
                     "atm_shortage",
                     "vacation")
      topic_ATM_Fraud <- as.factor(rep("EL0103_ATM_Fraud", length(ATM_Fraud)))
      seeded_words <- data.frame(topic = c(topic_Client_Account, topic_Unfaithfulness, topic_ATM_Fraud), 
                                 word = c(Client_Account, Unfaithfulness, ATM_Fraud))
    }
    if(ET.Filter[i] == "EL0200 - External Fraud"){
      Internet_Card_Fraud <- c("card_transaction_steal",
                     "steal_credit_card",
                     "credit_card_data",
                     "transaction_steal_credit",
                     "transaction_unauthorized_client",
                     "card_data_transaction",
                     "data_transaction_unauthorized",
                     "payment_productservice_internet",
                     "productservice_internet_provider",
                     "internet_provider_abroad",
                     "client_credit_card",
                     "credit_card_internet",
                     "unauthorized_client_credit")
      topic_Internet_Card_Fraud <- as.factor(rep("EL0201_Internet_Card_Fraud", length(Internet_Card_Fraud)))           
      Unauthorized_Card_Transaction_EEA <- c("unauthorized_card_transaction",
                     "card_transaction_execute",
                     "execute_eea_investigation",
                     "transaction_execute_eea",
                     "claim_recover_client",
                     "investigation_claim_recover",
                     "eea_investigation_claim")
      topic_Unauthorized_Card_Transaction_EEA <- as.factor(rep("EL0202_Unauthorized_Card_Transaction_EEA", 
                                                               length(Unauthorized_Card_Transaction_EEA)))
      Card_Cloning <- c("card_frad_cloning")
      topic_Card_Cloning <- as.factor(rep("EL0203_Card_Cloning", 
                                                               length(Card_Cloning)))
      seeded_words <- data.frame(topic = c(topic_Internet_Card_Fraud, topic_Unauthorized_Card_Transaction_EEA,
                                           topic_Card_Cloning),
                                 word = c(Internet_Card_Fraud, Unauthorized_Card_Transaction_EEA, Card_Cloning))
    }
    if(ET.Filter[i] == "EL0300 - Employment Practices and Workplace Safety"){
      Work_Injury = c("work_injury", "injury") 
      topic_Work_Injury <- as.factor(rep("EL0301_Work_Injury", length(Work_Injury)))
      Former_Employee_Litigation = c("former_employee_litigation",
                                     "legal_procedure_non-competition",
                                     "dipendente_legal_procedure",
                                     "non-competitio")
      topic_Former_Employee_Litigation <- as.factor(rep("EL0302_Former_Employee_Litigation", 
                                                        length(Former_Employee_Litigation)))
      Demotion = c("demotion", "compensatory")
      topic_Demotion <- as.factor(rep("EL0303_Demotion", 
                                                        length(Demotion)))
      seeded_words <- data.frame(topic = c(topic_Work_Injury, topic_Former_Employee_Litigation, topic_Demotion),
        word = c(Work_Injury, Former_Employee_Litigation, Demotion))
    }
    if(ET.Filter[i] == "EL0400 - Clients, Products & Business Practices"){
      CHF_Loans_Bulk = c("chf_loan", "loan_chf", "chf", "clause_chf_house",
                    "variable_interest_rate", 
                    "bank_apliance_currency", "apliance_currency_close", "currency_close_variable", 
                    "claim_repayment_overpay", "repayment_overpay_loan",
                    "consumer_house_loan", "house_loan_claim",
                    "interest_rate_consumer", "client_dispute_appliance",
                    "legal_dispute_client")
      topic_CHF_Loans_Bulk <- as.factor(rep("EL0401_CHF_Loans_Bulk", length(CHF_Loans_Bulk)))
                    
      CHF_Loans_Other = c("chf_loan", # Also present above
                    "client_benefit", "initiated_procedure_bank",
                    "procedure_bank_dispute", "bank_dispute_validity",
                    "dispute_validity_variable", "validity_variable_interest",
                    "variable_interest_rate", # Also present above
                    "interest_rate_clause",
                    "rate_clause_validity", "clause_validity_currency",
                    "validity_currency_clause", "currency_clause_client",
                    "clause_client_claim", "client_claim_repayment",
                    "claim_repayment_overpay") # Also present above
      topic_CHF_Loans_Other <- as.factor(rep("EL0402_CHF_Loans_Other", length(CHF_Loans_Other)))
      Anatocism = c("anatocism", "usury",
                    "overdraft_interest_rate", 
                    "usury_conto_corrente", 
                    "usury_data_accadimento",
                    "usury_variata_tipo") 
      topic_Anatocism <- as.factor(rep("EL0403_Anatocism", length(Anatocism)))
      Personal_Loan_Reimbursement = c("ucifin_branch_reimbursement",
                    "branch_reimbursement_open", 
                    "reimbursement_open_fee", 
                    "open_fee_person",
                    "fee_person_loan")
      topic_Personal_Loan_Reimbursement <- as.factor(rep("EL0404_Personal_Loan_Reimbursement", 
                                                         length(Personal_Loan_Reimbursement)))
      Derivatives_Misselling = c("derivatives_finance_instruments",
                                 "finance_instrument_lack",
                                 "instrument_lack_information",
                                 "lack_information_client",
                                 "corporates_division_client",
                                 "division_client_claim",
                                 "client_claim_compensation",
                                 "claim_compensation_miscounselling",     
                                 "compensation_miscounselling_connection", 
                                 "miscounselling_connection_derivatives", 
                                 "connection_derivatives_dealings",        
                                 "derivatives_dealings_bank")             
      topic_Derivatives_Misselling <- as.factor(rep("EL0405_Derivatives_Misselling", 
                                                         length(Derivatives_Misselling)))
      Client_Account = c("client_account",
                                 "claim_damage"
                                 )             
      topic_Client_Account <- as.factor(rep("EL0406_Client_Account", 
                                                    length(Client_Account)))
      seeded_words <- data.frame(topic = c(topic_CHF_Loans_Bulk, topic_CHF_Loans_Other, topic_Anatocism,
                                           topic_Personal_Loan_Reimbursement, topic_Derivatives_Misselling,
                                           topic_Client_Account),
                                 word = c(CHF_Loans_Bulk, CHF_Loans_Other, Anatocism,
                                          Personal_Loan_Reimbursement, Derivatives_Misselling,
                                          Client_Account))
    }
    if(ET.Filter[i] == "EL0500 - Damage to Physical Assets"){
      ATM_EE = c("atm",
                 "atm_locate",
                 "vandalism_bgn_pay",
                 "break_vandalism_bgn",
                 "damage_bgn_pay",
                 "atm_locate_sofia",
                 "keypad_break_vandalism",
                 "bgn_pay_repair",
                 "claim_insurer_insurance",
                 "insurer_insurance_payment",
                 "repair_claim_insurer",
                 "insurance_payment_expect",
                 "payment_expect_insurance",
                 "expect_insurance_payment",
                 "insurance_payment_bgn")
      topic_ATM_EE <- as.factor(rep("EL0501_ATM_EE", length(ATM_EE)))
      Car_Damage = c("car_damage",
                 "car_accident",
                 "casco_case_damage",
                 "case_damage_vehicle",
                 "vehicle_damage_error",
                 "vehicle_damage_party",
                 "damage_party_error",
                 "car",
                 "company_car_damage",
                 "casco",
                 "car_park")
      topic_Car_Damage <- as.factor(rep("EL0502_Car_Damage", length(Car_Damage)))
      seeded_words <- data.frame(topic = c(topic_ATM_EE, topic_Car_Damage),
                                 word = c(ATM_EE, Car_Damage))
    }
    if(ET.Filter[i] == "EL0600 - Business Disruption and System Failures"){
      Software_Bugs = c("cause_software_bug",
                        "error_cause_software",
                        "software_bug",
                        "bug")
      topic_Software_Bugs <- as.factor(rep("EL0601_Software_Bugs", length(Software_Bugs)))
      Loss_IT_Problem = c("loss_problem")
      topic_Loss_IT_Problem <- as.factor(rep("EL0602_Loss_IT_Problem", length(Loss_IT_Problem)))
      Digital_Payment_Processes = c("card",
                                    "digital_payment_procedure",
                                    "acquire_digital_payment",
                                    "payment_procedure_copia",
                                    "procedure_copia_conoscenza")
      topic_Digital_Payment_Processes <- as.factor(rep("EL0603_Digital_Payment_Processes", length(Digital_Payment_Processes)))
      seeded_words <- data.frame(topic = c(topic_Software_Bugs, topic_Loss_IT_Problem, topic_Digital_Payment_Processes),
                                 word = c(Software_Bugs, Loss_IT_Problem, Digital_Payment_Processes))
    }
    if(ET.Filter[i] == "EL0700 - Execution, Delivery & Process Management"){
      Inadequate_Data = c("return_inadequate_data",
                        "inadequate_data_concern",
                        "bank_record_statement",
                        "address_bank_record",
                        "client_record_bank",
                        "concern_client_address",
                        "client_address_bank",
                        "record_statement_corporate",
                        "record_statement_retail",
                        "adresses_bank_record",
                        "statement_return_inadequate",
                        "statement_retail_client",
                        "bank_cause_expense",
                        "client_return_bank",
                        "data_concern_client")
      topic_Inadequate_Data <- as.factor(rep("EL0701_Inadequate_Data", length(Inadequate_Data)))
      Cash_Differences = c("cash", "cash_desk", "reconcile", "different", "cash_transfer",
                           "cash_klrung")
      topic_Cash_Differences <- as.factor(rep("EL0702_Cash_Differences", length(Cash_Differences)))
      Client_Account = c("client_account") 
      topic_Client_Account <- as.factor(rep("EL0703_Client_Account", length(Client_Account)))
      Error_False_Notification = c("error_false_notification")
      topic_Error_False_Notification <- as.factor(rep("EL0704_Error_False_Notification", 
                                                      length(Error_False_Notification)))
      seeded_words <- data.frame(topic = c(topic_Inadequate_Data, topic_Cash_Differences,
                                           topic_Client_Account, topic_Error_False_Notification),
                                 word = c(Inadequate_Data, Cash_Differences, Client_Account,
                                          Error_False_Notification))
    }
  
  }
  
  toc()
  
  if(Seeded_tf){
    Semantic_tfidf_ET[, colnames(Semantic_tfidf_ET) %in% seeded_words$word] <- 
      Semantic_tfidf_ET[, colnames(Semantic_tfidf_ET) %in% seeded_words$word] * SeedWeight_tf
  }
  
  print(paste0("Estimate seeded LDA of ", ET.Filter[i]))
  tic()
  
  ii <- jj <- ww <- c()
  
  ProgressiveSeed <- FALSE # Not used (keep it FALSE)
  ProgressiveFactor <- 10
  
  for(w in 1:nrow(seeded_words)){
    word <- seeded_words[w,]
    sw <- which(Semantic_tfidf_ET@Dimnames[[2]] == word[,"word"])
    ii <- c(ii, rep(as.integer(word[,"topic"]),length(sw)))
    jj <- c(jj, sw)
    ww <- c(ww, SeedWeight / (ProgressiveFactor^(w-1)))
  }
  
  ordering <- order(ii)
  
  
  jj <- jj[ordering]
  ii <- ii[ordering]
  ww <- ww[ordering]
  
  if(ProgressiveSeed){
    v_seed <- ww
  }else{
    SeedWeightTopic <- SeedWeight 
    v_seed <- rep(SeedWeightTopic, length(ii))
  }
  
  
  deltaS <- slam::simple_triplet_matrix(ii, jj, v = v_seed,
                                        nrow = k, ncol = ncol(Semantic_tfidf_ET))
  
  dtm <- convert(Semantic_tfidf_ET, "topicmodels")
  
  # For LDA, input matrix needs to contain integer entries
  dtm$v <- round(dtm$v)

  LDA_verbose <- round(num.iterations_par[i] / 100)
  LDA_keep <- max(LDA_verbose / 100, 1)
  
  #tic()
  
  model[[i]] <- topicmodels::LDA(x = dtm, 
                            k = k, 
                            method = "Gibbs", 
                            seedwords = deltaS,
                            control = list(alpha = alpha_par[i],
                                           estimate.beta = TRUE,
                                           best = TRUE, 
                                           seed = seed, 
                                           delta = deltaW,
                                           verbose = LDA_verbose, 
                                           burnin = burn_par[i], 
                                           iter = num.iterations_par[i],
                                           thin = num.iterations_par[i], 
                                           keep = LDA_keep,
                                           prefix = getwd()
                                           )
                            )
  
  #toc()
  
  # Posterior probabilities
  post_lda <- posterior(model[[i]])
  post_lda_topics <-  post_lda$topics
  post_lda_terms <-  post_lda$terms
  
  # Traceplot
  LDA_logLiks <- model[[i]]@logLiks

  ylim_ET <- c(min(LDA_logLiks), max(LDA_logLiks))
  
  # plot(LDA_logLiks)
  LDA_logliks_ind <- seq(LDA_keep, burn_par[i]+num.iterations_par[i], by = LDA_keep)
  plot(LDA_logliks_ind, LDA_logLiks, xlab = "Iteration", ylab = "Log-likelihood", 
       pch = 16, 
       lty = 1,
       col = "blue", type = "l", # "b",
       ylim = ylim_ET)
  title(paste0("Traceplot of log-likelihood value for ET ", i))
  abline(v = burn_par[i], lty = 2)

  model[[i]]@iter
  model[[i]]@gamma
  
  toc()
  
  
  if(perp.calc){
    print(paste0("Perplexity of ", ET.Filter[i]))
    tic()
    training.set <- sample(rownames(dtm),round(nrow(dtm) * trainRatio))
    testing.dtm <- dtm[!rownames(dtm) %in% training.set,]
    training.dtm <- dtm[rownames(dtm) %in% training.set,]
    training.model <- topicmodels::LDA(x = training.dtm, 
                          k = k, 
                          method = "Gibbs", 
                          seedwords = deltaS,
                          control = list(alpha = alpha_par[i], 
                                         best = TRUE, seed = seed,
                                         verbose = LDA_verbose, 
                                         burnin = burn_par[i], iter = num.iterations_par[i],
                                         thin = num.iterations_par[i], prefix = getwd()))
    perp.ET$Perplexity[i] <- topicmodels::perplexity(training.model, testing.dtm,
                                                     control = list(alpha = alpha_par[i], 
                                                                    best = TRUE, seed = seed,
                                                                    verbose = LDA_verbose, 
                                                                    burnin = burn_par[i], iter = num.iterations_par[i],
                                                                    thin = num.iterations_par[i], prefix = getwd()))
    toc()
  }
  
  print(paste0("Topics probability of ", ET.Filter[i]))
  
  tic()
  
  topicmodels::terms(model[[i]], 10)
  table(topicmodels::topics(model[[i]]))
  
  
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
  
  dw <- t(distinctive_words(model[[i]]))
  colnames(dw) <- as.character(1:k)
  
  ProbTopics[[i]] <- model[[i]]@gamma
  colnames(ProbTopics[[i]]) <- as.character(1:k)
  rownames(ProbTopics[[i]]) <- model[[i]]@documents
  
  ProbTopicsNoAvg[[i]] <- ProbTopics[[i]]
  
  toc()
  
  if(Averaged_LDA){
    print(paste0("Averaging topic probabilities of ", ET.Filter[i]))
    tic()
    ProbTopics_Orig <- ProbTopics[[i]]
    nrow_ProbTopics <- nrow(ProbTopics[[i]])
    for(i_doc in 1:nrow_ProbTopics){
      #print(i_doc)
      tf_doc <- Semantic_tfidf_ET[i_doc,]
      tf_doc_equal <- textstat_dist(Semantic_tfidf_ET, tf_doc, method = "euclidean")==0
      tf_doc_equal <- as.logical(tf_doc_equal)
      tf_doc_sim <- dfm_subset(Semantic_tfidf_ET, tf_doc_equal )
      ProbTopics_set <- ProbTopics_Orig[rownames(ProbTopics_Orig) %in% rownames(tf_doc_sim), , drop=F]
      ProbTopics[[i]][i_doc,] <- apply(ProbTopics_set, 2, mean)
    }
    toc()
  }
  
  if(Averaged_LDA_Loaded){
    load(file = file.path(path, paste0("ProbTopics_ET", i, ".Rdata")))
  }
  
  ProbWords[[i]] <- t(exp(model[[i]]@beta))
  row.names(ProbWords[[i]]) <- model[[i]]@terms
  
  if(wordcloud_plot){
    
    print(paste0("Wordcloud of ", ET.Filter[i]))
    tic()
  
    def.par <- par(no.readonly = TRUE)
    for (topic in 1:k) {
      df <- data.frame(term = row.names(ProbWords[[i]]), p = ProbWords[[i]][, topic])
      head(df[order(-df$p), ])
      
      layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
      par(mar = rep(0, 4))
      plot.new()
      text(x = 0.5,
           y = 0.5,
           paste0("ET ", ET.Filter[i], " - Topic ", topic)) 
      
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
    
  }
  
  # Get the top terms of each topic
  print("Top terms of the topics")
  top_terms <- GetTopTerms(phi = t(ProbWords[[i]]), M = 10)
  print(ET.Filter[i])
  print(top_terms)
  
  print(paste0("Assign clusters to decriptions of ", ET.Filter[i]))
  tic()

  # Select filtered toks
  toks_Corep_ET <- toks_Corep[names(toks_Corep) %in% rownames(ProbTopics[[i]]),]
  
  # Assign the prevalent topic to each description
  nrow.ProbTopics <- nrow(ProbTopics[[i]])
  if(Constrainted){
    seeded_words$IndTopic <- as.numeric(substr(seeded_words$topic, 5 , 6))
    ListTopics <- as.list(rep(0, nrow.ProbTopics))
    
    for(j in 1:nrow.ProbTopics){
      ListTopics[[j]] <- 1:k  
    }
    
    for(j in 1:nrow.ProbTopics){
      #print(j)
      temp <- which(seeded_words$word %fin% toks_Corep_ET[[j]])
      if(length(temp)>0){
        ListTopics[[j]] <- seeded_words$IndTopic[temp]
      }
    }
    temp <- rep(0, nrow.ProbTopics)
    for(j in 1:nrow.ProbTopics){
      #print(j)
      temp[j] <- max(ProbTopics[[i]][j,ListTopics[[j]]])
    }
    
    model_cluster_vec <- rep(0, length(temp))
    for(j in 1:length(temp)) {
      # Identify topic with max probability
      ProbTopicsConstr <- ProbTopics[[i]][j, ListTopics[[j]], drop=F]
      model_cluster_vec[j] <- colnames(ProbTopicsConstr[, ProbTopicsConstr == temp[j], drop=F])[1]
    }
    
  }else{
  
    temp <- apply(ProbTopics[[i]] , 1, max)
    model_cluster_vec <- rep(0, length(temp))
    for(j in 1:length(temp)) {
      # Identify topic with max probability
      model_cluster <- names(ProbTopics[[i]][j, ][ProbTopics[[i]][j, ] == temp[j]])
      # In case of ties, assign the residual topic
      if (length(model_cluster) > 1) {
        model_cluster_vec[j] <- residualTopic.ET.Filter[i]
      }else{
        model_cluster_vec[j] <- which(colnames(ProbTopics[[i]]) == model_cluster)
      }
    }
    
  }
  
  model_cluster_vec <- as.numeric(model_cluster_vec)
  table(model_cluster_vec)
  
  OpData_ET$Cluster <- model_cluster_vec
  
  
  prob_words_residual_topic <- 0.5
  n_other_words_max <- 30
  
  # To be used in case Seeds="InternalData" in Twitter analysis (not used for the thesis).
  # As seed to be used for residual topic (to be used for Twitter data),
  # we consider the highest probability words of the residual topic
  # which have not been already included in the other topics.
  # The number of seeded words in the residual topic is decided
  # as the min btw (rounded) average of the number of seeds for all seeded topics,
  # and the number of words cumulating at least the probability prob_words_residual_topic,
  # but no more than n_other_words_max words
  seeded_words_complete <- rbind(seeded_words_complete, seeded_words[,1:2])
  n_other_words_min <- round(nrow(seeded_words) / nTopics.ET.Filter[i])
  other_words <- ProbWords[[i]][,residualTopic.ET.Filter[i]]
  other_words <- other_words[!names(other_words) %in% seeded_words$word]
  other_words_prob <- sort(other_words, decreasing = T)
  other_words_prob_cum <- cumsum(other_words_prob)
  n_other_words <- length(other_words_prob_cum[other_words_prob_cum <= prob_words_residual_topic])
  n_other_words <- max(n_other_words, n_other_words_min)
  n_other_words <- min(n_other_words, n_other_words_max)
  other_words <- other_words_prob_cum[1:n_other_words]
  other_words <- names(other_words)

  
  if (ET.Filter[i] == "EL0100 - Internal Fraud") {
    OpData_ET$Cluster[OpData_ET$Cluster == 1] <- "EL0101 - Client Account"
    OpData_ET$Cluster[OpData_ET$Cluster == 2] <- "EL0102 - Unfaithfulness"
    OpData_ET$Cluster[OpData_ET$Cluster == 3] <- "EL0103 - ATM Fraud"
    OpData_ET$Cluster[OpData_ET$Cluster == 4] <- "EL0100 - IF - Other"
    other_topic <- rep("EL0100_Internal_Fraud_Other", length(other_words))
  }
  if (ET.Filter[i] == "EL0200 - External Fraud") {
    OpData_ET$Cluster[OpData_ET$Cluster == 1] <- "EL0201 - Internet Card Fraud"
    OpData_ET$Cluster[OpData_ET$Cluster == 2] <- "EL0202 - Unauthorized Card Transaction EEA"
    OpData_ET$Cluster[OpData_ET$Cluster == 3] <- "EL0203 - Card Cloning"
    OpData_ET$Cluster[OpData_ET$Cluster == 4] <- "EL0200 - EF - Other"
    other_topic <- rep("EL0200_External_Fraud_Other", length(other_words))
  }
  if (ET.Filter[i] == "EL0300 - Employment Practices and Workplace Safety") {
    OpData_ET$Cluster[OpData_ET$Cluster == 1] <- "EL0301 - Work Injury"
    OpData_ET$Cluster[OpData_ET$Cluster == 2] <- "EL0302 - Former Employee Litigation"
    OpData_ET$Cluster[OpData_ET$Cluster == 3] <- "EL0303 - Demotion"
    OpData_ET$Cluster[OpData_ET$Cluster == 4] <- "EL0300 - EP&WS - Other"
    other_topic <- rep("EL0300_Employment_Practices_&_Workplace_Safety_Other", length(other_words))
  }
  if (ET.Filter[i] == "EL0400 - Clients, Products & Business Practices") {
    OpData_ET$Cluster[OpData_ET$Cluster == 1] <-
      "EL0401 - CHF Loans Bulk"
    OpData_ET$Cluster[OpData_ET$Cluster == 2] <-
      "EL0402 - CHF Loans Other"
    OpData_ET$Cluster[OpData_ET$Cluster == 3] <-
      "EL0403 - Anatocism"
    OpData_ET$Cluster[OpData_ET$Cluster == 4] <-
      "EL0404 - Personal Loan Reimbursement"
    OpData_ET$Cluster[OpData_ET$Cluster == 5] <-
      "EL0405 - Derivatives Misselling"
    OpData_ET$Cluster[OpData_ET$Cluster == 6] <-
      "EL0406 - Client Account"
    OpData_ET$Cluster[OpData_ET$Cluster == 7] <- "EL0400 - CP&BP - Other"
    other_topic <- rep("EL0400_Clients_Products_&_Business_Practices_Other", length(other_words))
  }
  if (ET.Filter[i] == "EL0500 - Damage to Physical Assets") {
    OpData_ET$Cluster[OpData_ET$Cluster == 1] <- "EL0501 - ATM EE"
    OpData_ET$Cluster[OpData_ET$Cluster == 2] <- "EL0502 - Car Damage"
    OpData_ET$Cluster[OpData_ET$Cluster == 3] <- "EL0500 - DPA - Other"
    other_topic <- rep("EL0500_Damage_to_Physical_Assets_Other", length(other_words))
  }
  if (ET.Filter[i] == "EL0600 - Business Disruption and System Failures") {
    OpData_ET$Cluster[OpData_ET$Cluster == 1] <-
      "EL0601 - Software Bugs"
    OpData_ET$Cluster[OpData_ET$Cluster == 2] <-
      "EL0602 - Loss IT Problem"
    OpData_ET$Cluster[OpData_ET$Cluster == 3] <-
      "EL0603 - Digital Payment Processes"
    OpData_ET$Cluster[OpData_ET$Cluster == 4] <- "EL0600 - BD&SF - Other"
    other_topic <- rep("EL0600_Business_Disruption_&_System_Failures_Other", length(other_words))
  }
  if (ET.Filter[i] == "EL0700 - Execution, Delivery & Process Management") {
    OpData_ET$Cluster[OpData_ET$Cluster == 1] <-
      "EL0701 - Inadequate Data"
    OpData_ET$Cluster[OpData_ET$Cluster == 2] <-
      "EL0702 - Cash Differences"
    OpData_ET$Cluster[OpData_ET$Cluster == 3] <-
      "EL0703 - Client Account"
    OpData_ET$Cluster[OpData_ET$Cluster == 4] <-
      "EL0704 - Error False Notification"
    OpData_ET$Cluster[OpData_ET$Cluster == 5] <- "EL0700 - ED&PM - Other"
    other_topic <- rep("EL0700_Execution_Delivery_&_Process_Management_Other", length(other_words))
  }
  
  other_seeded_words <- data.frame(topic = other_topic, word = other_words)
  seeded_words_complete <- rbind(seeded_words_complete[,1:2], other_seeded_words)
  
  OpData.Filter$Cluster[which(OpData.Filter$ID %in% OpData_ET$ID)] <- OpData_ET$Cluster
  
  toc()
  
}

print("Save all results by Event Type")

tic()

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(model, file = file.path(path, paste0("model_LDA_ET_", date, ".Rdata")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(ProbTopicsNoAvg, file = file.path(path, paste0("ProbTopicsNoAvg_ET_", date, ".Rdata")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(ProbTopics, file = file.path(path, paste0("ProbTopics_ET_", date, ".Rdata")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(ProbWords, file = file.path(path, paste0("ProbWords_ET_", date, ".Rdata")))

gc()
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(Semantic_pca_ET, file = file.path(path, paste0("Semantic_pca_ET_", date, ".RData")))
gc()

print("Number of events for each cluster")
table(OpData.Filter$Cluster)

if(perp.calc){
  print(perp.ET)
}

toc()


# PCA by ET with clusters ####
print("PCA by ET with clusters")
tic()
l <- list()
df_pca_ET_cluster <- list()
for(j_ET in 1:length(ET)){
  OpData_ET <- OpData.Filter[OpData.Filter$'Event Type' == ET[j_ET],]
  Semantic_tfidf_ET <- Semantic_tfidf.Filter[row.names(Semantic_tfidf.Filter) %in% OpData_ET$ID,]
  df_pca_ET_cluster[[j_ET]] <- data.frame(x=Semantic_pca_ET[[j_ET]]$x[, 1], y=Semantic_pca_ET[[j_ET]]$x[, 2],
                          id_desc=paste(rownames(Semantic_tfidf_ET), OpData_ET$Description_v3,
                                        "\u000A\ \u000A\ CLEANED:", OpData_ET$Description_Cleaned),
                          cluster=OpData_ET$Cluster)

  l[[j_ET]] <- plot_ly(df_pca_ET_cluster[[j_ET]], x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers",
                       color = ~cluster) %>%
    layout(title = paste0("PCA of ", ET[j_ET]))
}
l
toc()

# LSA by ET with clusters ####
print("LSA by ET with clusters")
tic()
l <- list()
df_lsa_ET_cluster <- list()
for(j_ET in 1:length(ET)){
  OpData_ET <- OpData.Filter[OpData.Filter$'Event Type' == ET[j_ET],]
  Semantic_tfidf_ET <- Semantic_tfidf.Filter[row.names(Semantic_tfidf.Filter) %in% OpData_ET$ID,]
  df_lsa_ET_cluster[[j_ET]] <- data.frame(x=Semantic_lsa.Filter.ET[[j_ET]]$docs[, 1], y=Semantic_lsa.Filter.ET[[j_ET]]$docs[, 2],
                          id_desc=paste(rownames(Semantic_tfidf_ET), OpData_ET$Description_v3,
                                        "\u000A\ \u000A\ CLEANED:", OpData_ET$Description_Cleaned),
                          cluster=OpData_ET$Cluster)

  l[[j_ET]] <- plot_ly(df_lsa_ET_cluster[[j_ET]], x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers",
                    color = ~cluster) %>%
    layout(title = paste0("LSA of ", ET[j_ET]))
}
l
toc()


# UMAP by ET with clusters ####
print("UMAP by ET with clusters")
tic()
l <- list()
df_umap_ET_cluster <- list()
#Semantic_tfidf.Filter.umap.ET <- list()
for(j_ET in 1:length(ET)){
  print(j_ET)
  OpData_ET <- OpData.Filter[OpData.Filter$'Event Type' == ET[j_ET],]
  Semantic_tfidf_ET <- Semantic_tfidf.Filter[row.names(Semantic_tfidf.Filter) %in% OpData_ET$ID,]
  
  layout <- Semantic_tfidf.Filter.umap.ET[[j_ET]]
  layout <- data.frame(layout)
  df_umap_ET_cluster[[j_ET]] <- cbind(layout, 
                   id_desc=paste(OpData_ET$ID, OpData_ET$Description_v3,
                                 "\u000A\ \u000A\ CLEANED:", OpData_ET$Description_Cleaned),
                   cluster=OpData_ET$Cluster
                   
  )
  
  df_umap_ET_cluster[[j_ET]] <- as.data.frame(df_umap_ET_cluster[[j_ET]])
  
  l[[j_ET]] <- plot_ly(df_umap_ET_cluster[[j_ET]], x = ~X1, y = ~X2, 
                    text = ~id_desc, 
                    type = 'scatter', mode = 'markers',
                    color = ~cluster) %>%
    layout(
      plot_bgcolor = "#e5ecf6",
      xaxis = list(
        title = "V1"),
      yaxis = list(
        title = "V2"),
      title = paste0("UMAP of ", ET[j_ET]))
}
l
toc()

gc()

print("Save overall results")
tic()

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_pca_ET_cluster, file = file.path(path, paste0("df_pca_ET_cluster_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_lsa_ET_cluster, file = file.path(path, paste0("df_lsa_ET_cluster_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(df_umap_ET_cluster, file = file.path(path, paste0("df_umap_ET_cluster_", date, ".RData")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(seeded_words_complete, file = file.path(path, paste0("seeded_words_complete_", date, ".Rdata")))

date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
save(OpData.Filter, file = file.path(path, paste0("OpData.Filter_", date, ".Rdata")))


if(perp.calc){
  date <- Sys.time()
  date <- as.character(date)
  date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
  save(perp.ET, file = file.path(path, paste0("perp.ET_", trainRatio*100, "_", date, ".Rdata")))
}

toc()

gc()


}

print("End of calculation")

sink()

