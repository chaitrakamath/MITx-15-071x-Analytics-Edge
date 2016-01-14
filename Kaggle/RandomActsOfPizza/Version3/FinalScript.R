rm(list = ls())
getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Kaggle/RandomActsOfPizza/Version3")
getwd()

#install.packages('rjson')
library(rjson)

#Step1: Convert json file to data frame. Found the code for this here:
#https://www.kaggle.com/benhamner/random-acts-of-pizza/rmarkdown-default-text
trainJSON = fromJSON(file = 'train.json')
testJSON = fromJSON(file = 'test.json')
# Helper functions for converting the JSON data to a dataframe
processCell <- function(cell) {
        if (is.null(cell) || length(cell) == 0) {
                return(NA)
        }
        if (length(cell)>1) {
                return(paste(cell, collapse='; '))    
        }
        return(cell)
}
processRow <- function(row) unlist(lapply(row, processCell))

train <- as.data.frame(do.call("rbind", lapply(trainJSON, processRow)))
str(train)

test = as.data.frame(do.call('rbind', lapply(testJSON, processRow)))
str(test)


#Data preprocessing
numNames = c("number_of_downvotes_of_request_at_retrieval", "number_of_upvotes_of_request_at_retrieval", 
             "request_number_of_comments_at_retrieval", "requester_account_age_in_days_at_request" , 
             "requester_account_age_in_days_at_retrieval", "requester_days_since_first_post_on_raop_at_request", 
             "requester_days_since_first_post_on_raop_at_retrieval", "requester_number_of_comments_at_request" ,
             "requester_number_of_comments_in_raop_at_retrieval", "requester_number_of_posts_at_request",
             "requester_number_of_posts_at_retrieval", "requester_number_of_posts_on_raop_at_request" , 
             "requester_number_of_posts_on_raop_at_retrieval" , "requester_number_of_subreddits_at_request" ,
             "requester_upvotes_minus_downvotes_at_request", "requester_upvotes_minus_downvotes_at_retrieval",
             "requester_upvotes_plus_downvotes_at_request"  , "requester_upvotes_plus_downvotes_at_retrieval",
             "requester_number_of_comments_at_retrieval", "requester_number_of_comments_in_raop_at_request",
             "request_id")
train[, numNames] = lapply(train[,numNames],as.numeric)
str(train)

charNames = c("giver_username_if_known", "request_text", "request_text_edit_aware",
              "request_title", "requester_subreddits_at_request", "requester_username")
train[,charNames] = lapply(train[,charNames], as.character)
str(train)

numNamesTest = c("requester_account_age_in_days_at_request", "requester_days_since_first_post_on_raop_at_request",
                 "requester_number_of_comments_at_request","requester_number_of_comments_in_raop_at_request", 
                 "requester_number_of_posts_at_request","requester_number_of_posts_on_raop_at_request", 
                 "requester_number_of_subreddits_at_request","requester_upvotes_minus_downvotes_at_request",
                 "requester_upvotes_plus_downvotes_at_request")
test[, numNamesTest] = lapply(test[, numNamesTest], as.numeric)

charNamesTest = c("request_id", "giver_username_if_known" , "request_text_edit_aware", "request_title", 
                  "requester_subreddits_at_request", "requester_username")
test[, charNamesTest] = lapply(test[, charNamesTest], as.character)


#Baseline Model - If giver username is known, the requester received pizza
#predict TRUE if giver_username_if_known != 'N/A' else FALSE
table(test$giver_username_if_known != "N/A") #113 givers have usernames known
predictions = ifelse(test$giver_username_if_known != "N/A", 1, 0)
table(predictions) #Corresponding 113 requester received pizza
submission = data.frame(request_id = test$request_id, requester_received_pizza = predictions)
write.csv(submission, 'baselineModel.csv', row.names = FALSE)

#Look at the data when giver username is not known and do analysis based on that
subTrain = subset(train, train$giver_username_if_known == "N/A")
subTest = subset(test, test$giver_username_if_known == "N/A")
unique(subTrain$requester_received_pizza)

#Create new subsets by removing numeric columns

subTrain$requester_received_pizza = as.factor(subTrain$requester_received_pizza)
unique(subTrain$requester_received_pizza)
charColumns = c("request_id", "giver_username_if_known" , "request_text_edit_aware", "request_title", 
                "requester_subreddits_at_request", "requester_username")
newSubTrain = subset(subTrain, select = charColumns)
newSubTrain$requester_received_pizza = subTrain$requester_received_pizza
str(newSubTrain)
newSubTest = subset(subTest, select = charColumns)
newSubTest$requester_received_pizza = NA
str(newSubTest)
newFullData = rbind(newSubTrain, newSubTest)
str(newFullData)

#Bag of words - Title
library(tm)
titleCorpus = Corpus(VectorSource(newFullData$request_title))
titleCorpus[[1]]
titleCorpus = tm_map(titleCorpus, tolower)
titleCorpus[[1]]
titleCorpus = tm_map(titleCorpus, PlainTextDocument)
titleCorpus[[1]]
titleCorpus = tm_map(titleCorpus, removePunctuation)
titleCorpus[[1]]
titleCorpus = tm_map(titleCorpus, removeWords, stopwords('English'))
titleCorpus[[1]]
titleCorpus = tm_map(titleCorpus, stemDocument)
titleCorpus[[1]]
titleDTM = DocumentTermMatrix(titleCorpus)
titleDTM
titleDTMSparse = removeSparseTerms(titleDTM, 0.98)
titleDTMSparse

titleTerms = as.data.frame(as.matrix(titleDTMSparse))
colnames(titleTerms) = paste0('title_', colnames(titleTerms))
colnames(titleTerms)
str(titleTerms)

#Bag of words - Text
textCorpus = Corpus(VectorSource(newFullData$request_text_edit_aware))
textCorpus[[1]]
textCorpus = tm_map(textCorpus, tolower)
textCorpus[[1]]
textCorpus = tm_map(textCorpus, removePunctuation)
textCorpus[[1]]
textCorpus = tm_map(textCorpus, removeWords, stopwords('English'))
textCorpus[[1]]
textCorpus = tm_map(textCorpus, PlainTextDocument)
textCorpus[[1]]
textCorpus = tm_map(textCorpus, stemDocument)
textCorpus[[1]]
textDTM = DocumentTermMatrix(textCorpus)
textDTM
textDTMSparse = removeSparseTerms(textDTM, 0.98)
textDTMSparse

textTerms = as.data.frame(as.matrix(textDTMSparse))
colnames(textTerms) = paste0('text_', colnames(textTerms))
colnames(textTerms)
str(textTerms)

fullDataBagOfWords = cbind(request_id = newFullData$request_id, titleTerms, textTerms, requester_received_pizza = newFullData$requester_received_pizza, row.names = NULL)
colnames(fullDataBagOfWords)

bagOfWordsTrain = subset(fullDataBagOfWords, !is.na(requester_received_pizza))
str(bagOfWordsTrain)
bagOfWordsTest = subset(fullDataBagOfWords, is.na(requester_received_pizza))
bagOfWordsTest$requester_received_pizza = NULL
str(bagOfWordsTest)

#Look at conditional probabilities and make predictions 
receivedTrain = subset(bagOfWordsTrain, bagOfWordsTrain$requester_received_pizza == TRUE)
receivedTest = subset(bagOfWordsTest, bagOfWordsTest$requester_received_pizza == TRUE)
str(receivedTrain$requester_received_pizza)
receivedTrain$requester_received_pizza = as.numeric(as.logical(receivedTrain$requester_received_pizza))
unique(receivedTrain$requester_received_pizza)
colnames(receivedTrain)
acceptedFreq = data.frame(acceptedFrequency = sort(colSums(receivedTrain[, 2:383]) / nrow(receivedTrain)))

table(bagOfWordsTrain$requester_received_pizza)
rejectedTrain = subset(bagOfWordsTrain, bagOfWordsTrain$requester_received_pizza == FALSE)
rejectedTest = subset(bagOfWordsTest, bagOfWordsTest$requester_received_pizza == FALSE)
colnames(rejectedTrain)
rejectedTrain$requester_received_pizza = as.numeric(as.logical(rejectedTrain$requester_received_pizza))
table(rejectedTrain$requester_received_pizza)
rejectedFreq = data.frame(rejectedFrequency = sort(colSums(rejectedTrain[, 2:383]) / nrow(rejectedTrain),
                                                   decreasing = TRUE))

intersectionFreq = merge(acceptedFreq, rejectedFreq, by = 'row.names')
intersectionFreq$diff = intersectionFreq$acceptedFrequency - intersectionFreq$rejectedFrequency
intersectionFreq[order(intersectionFreq$acceptedFrequency, -intersectionFreq$rejectedFrequency), ]
intersectionFreq[order(intersectionFreq$diff), ]




outcome = ifelse(bagOfWordsTest$text_day == 1 || bagOfWordsTest$text_thank == 1 ||
        bagOfWordsTest$text_get == 1 || bagOfWordsTest$text_pay == 1 || 
        bagOfWordsTest$text_help == 1 || bagOfWordsTest$text_pizza == 1 ||
        bagOfWordsTest$text_work == 1, 1, 0)

predictions = ifelse(test$giver_username_if_known != "N/A", 1, 0)
table(predictions) #Corresponding 113 requester received pizza
submission = data.frame(request_id = test$request_id, requester_received_pizza = predictions)
write.csv(submission, 'baselineModel.csv', row.names = FALSE)
