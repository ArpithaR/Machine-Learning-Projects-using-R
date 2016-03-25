## Bayesian Classification of SMS messages into spam and no-spam 

# Load libraries
library(tm)
library(klaR)
library(gmodels)

# Read input file
inp_sms_file <- read.delim("../SMSSpamCollection.txt",sep="\t",stringsAsFactors = FALSE)
colnames(inp_sms_file) <- c("type", "text")

### ---------------------------------------------------------------------------------------- ###
###                           Data PreProcessing                                             ###
### ---------------------------------------------------------------------------------------- ###

# Factorize type and create corpus for the text
inp_sms_file$type <- factor(inp_sms_file$type)
sms_corpus <- Corpus(VectorSource(inp_sms_file$text))

# Preparing data for analysis by converting all text to lowecase, removing numbers, removing stop words, etc
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))                  # convert to lowercase
sms_corpus_clean <- tm_map(sms_corpus_clean, content_transformer(removeNumbers))      # remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())                # remove stopwords
sms_corpus_clean <- tm_map(sms_corpus_clean, content_transformer(removePunctuation))  # remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, content_transformer(stripWhitespace))    # remove whitespaces

# Tokenize the message corpus and create a sparse matrix 
sms_dtm_mtx <- DocumentTermMatrix(sms_corpus_clean)

## Split data into train and test datasets in 75:25 ratio 
# 1) split input sms file
inp_sms_train <- inp_sms_file[1:2387,]
inp_sms_test <- inp_sms_file[2388:3183,]
# 2) split data term matrix
sms_dtm_train <- sms_dtm_mtx[1:2387,]
sms_dtm_test <- sms_dtm_mtx[2388:3183,]
# 3) split sms corpus
sms_corpus_train <- sms_corpus_clean[1:2387]
sms_corpus_test <- sms_corpus_clean[2388:3183]

# Keep only frequently occuring words
sms_train_file <- DocumentTermMatrix(sms_corpus_train, list(dictionary = findFreqTerms(sms_dtm_train,25)))
sms_test_file <- DocumentTermMatrix(sms_corpus_test, list(dictionary = findFreqTerms(sms_dtm_test,15)))

# Convert count to factors
convert_tofactor <- function(n) {
    n <- ifelse(n > 0, 1, 0)
    n <- factor(n, levels = c(0,1), labels = c("No", "Yes"))
    return(n)
}

# apply convert_tofactor function for the train and test datasets
sms_train_file <- apply(sms_train_file, MARGIN = 2, convert_tofactor)
sms_test_file <- apply(sms_test_file, MARGIN = 2, convert_tofactor)

### ---------------------------------------------------------------------------------------- ###
###                         Training the model                                               ###
### ---------------------------------------------------------------------------------------- ###
# Build classifierss
sms_train_model <- NaiveBayes(sms_train_file, inp_sms_train$type, laplace = 1)

sms_test_predictor <- predict(sms_train_model, sms_test_file)

# Check model performance
CrossTable(sms_test_predictor$class, inp_sms_test$type, prop.chisq = FALSE, prop.t = FALSE, 
           prop.r = FALSE, dnn = c('predicted', 'actual'))
           
