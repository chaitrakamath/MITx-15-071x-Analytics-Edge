rm(list = ls())
getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Kaggle/RandomActsOfPizza/Version1")
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

#
subTrain = subset(train, train$giver_username_if_known == "N/A")
subTest = subset(test, test$giver_username_if_known == "N/A")
unique(subTrain$requester_received_pizza)

#Correlation plot of subTrain numeric columns
# subTrain$requester_received_pizza = as.numeric(subTrain$requester_received_pizza)
# numericColumns = c("requester_account_age_in_days_at_request", "requester_days_since_first_post_on_raop_at_request",
#                    "requester_number_of_comments_at_request","requester_number_of_comments_in_raop_at_request", 
#                    "requester_number_of_posts_at_request","requester_number_of_posts_on_raop_at_request", 
#                    "requester_number_of_subreddits_at_request","requester_upvotes_minus_downvotes_at_request",
#                    "requester_upvotes_plus_downvotes_at_request", "requester_received_pizza")
# numericTrain = subset(subTrain, select = numericColumns)
# colnames(numericTrain)
# pairs(numericTrain) #Lookng at the plot shows that the numeric columns do not really impact outcome
# subTrain$requester_received_pizza = as.logical(subTrain$requester_received_pizza)
# unique(subTrain$requester_received_pizza)

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
titleDTMSparse = removeSparseTerms(titleDTM, 0.95)
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
textDTMSparse = removeSparseTerms(textDTM, 0.95)
textDTMSparse

textTerms = as.data.frame(as.matrix(textDTMSparse))
colnames(textTerms) = paste0('text_', colnames(textTerms))
colnames(textTerms)
str(textTerms)

fullDataBagOfWords = cbind(titleTerms, textTerms, requester_received_pizza = newFullData$requester_received_pizza, row.names = NULL)
str(fullDataBagOfWords)

bagOfWordsTrain = subset(fullDataBagOfWords, !is.na(requester_received_pizza))
str(bagOfWordsTrain)
bagOfWordsTest = subset(fullDataBagOfWords, is.na(requester_received_pizza))
bagOfWordsTest$requester_received_pizza = NULL
str(bagOfWordsTest)

#Create simple log Model
logModel = glm(requester_received_pizza ~ ., data = bagOfWordsTrain, family = binomial)
summary(logModel)

logPredictions = predict(logModel, newdata = bagOfWordsTest, type = 'response')
max(logPredictions)
logModelOutcomes = as.numeric(logPredictions >= 0.5)
unique(logModelOutcomes)

#if giver username is known, requester always receives pizza. Else, look up values of logModelOutcomes
predictions = ifelse(test$giver_username_if_known != "N/A", 1, logModelOutcomes)
unique(predictions)
submission = data.frame(request_id = test$request_id, requester_received_pizza = predictions)
write.csv(submission, 'simpleLogModel.csv', row.names = FALSE)

#Create CART Model on bag of words
library(rpart)
library(caret)
library(rpart.plot)

numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.001)) 

train(as.data.frame(bagOfWordsTrain[, 1:145]), bagOfWordsTrain$requester_received_pizza,
      data = bagOfWordsTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid, verbose = F)

pizzaCART = rpart(requester_received_pizza ~ ., data = bagOfWordsTrain, method = 'class')
prp(pizzaCART)
CARTOutcomes = predict(pizzaCART, newdata = bagOfWordsTest, type = 'class')
unique(CARTOutcomes)


#Random Forest Model
library(randomForest)
optimumMTry = tuneRF(x = bagOfWordsTrain[, 1:145], 
                     y = bagOfWordsTrain[, c('requester_received_pizza')], ntreeTry = 500)

pizzaRF = randomForest(requester_received_pizza ~ ., data = bagOfWordsTrain, mtry = 12, nodesize = 25,
                       ntree = 500)
predictionsRF = as.numeric(as.logical(predict(pizzaRF, newdata = bagOfWordsTest, type = 'response')))
probRF = predict(pizzaRF, newdata = bagOfWordsTest, type = 'prob')
summary(probRF)
unique(predictionsRF)

#if giver username is known, requester always receives pizza. Else, look up values of predictionsRF
predictions = ifelse(test$giver_username_if_known != "N/A", 1, predictionsRF)
unique(predictions)
submission = data.frame(request_id = test$request_id, requester_received_pizza = predictions)
write.csv(submission, 'simpleRFModel.csv', row.names = FALSE)

#Create ensemble of random forest and logistic regression models
ensembleProbabilities = (probRF[, 2] + logPredictions) / 2
summary(ensembleProbabilities)
ensembleOutcome = as.numeric(ensembleProbabilities >= 0.5)
unique(ensembleOutcome)

predictions = ifelse(test$giver_username_if_known != "N/A", 1, ensembleOutcome)
unique(predictions)
submission = data.frame(request_id = test$request_id, requester_received_pizza = predictions)
write.csv(submission, 'ensembleModel-Log+RF.csv', row.names = FALSE)
