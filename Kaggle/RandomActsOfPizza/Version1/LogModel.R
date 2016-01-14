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

#Simple Log Model w/o text variables
simpleModel = glm(requester_received_pizza ~ 
                          requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + 
                          requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + 
                          requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + 
                          requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request +         
                          requester_upvotes_plus_downvotes_at_request , data = train, family = binomial )

summary(simpleModel)

predictions = predict(simpleModel, newdata = test, type = 'response')
head(predictions)
binomialPred = as.numeric(predictions > 0.5)
head(binomialPred)

#Create bag of words
library(tm)

test$requester_received_pizza = NA
newTrain = subset(train, select = colnames(test))
newTrain$requester_received_pizza = train$requester_received_pizza
fullData = rbind(newTrain, test)
str(fullData)


textCorpus = Corpus(VectorSource(fullData$request_text_edit_aware))
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


subredditCorpus = Corpus(VectorSource(fullData$requester_subreddits_at_request))
subredditCorpus[[1]]
subredditCorpus = tm_map(subredditCorpus, tolower)
subredditCorpus[[1]]
subredditCorpus = tm_map(subredditCorpus, PlainTextDocument)
subredditCorpus[[1]]
subredditCorpus = tm_map(subredditCorpus, removePunctuation)
subredditCorpus[[1]]
subredditCorpus = tm_map(subredditCorpus, removeWords, stopwords('English'))
subredditCorpus[[1]]
subredditCorpus = tm_map(subredditCorpus, stemDocument)
subredditCorpus[[1]]
subredditDTM = DocumentTermMatrix(subredditCorpus)
subredditDTM
subredditDTMSparse = removeSparseTerms(subredditDTM, 0.95)
subredditDTMSparse

subredditTerms = as.data.frame(as.matrix(subredditDTMSparse))
colnames(subredditTerms) = paste0('subreddit_', colnames(subredditTerms))
colnames(subredditTerms)
str(subredditTerms)


titleCorpus = Corpus(VectorSource(fullData$request_title))
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

trainBagOfWords = cbind(textTerms, subredditTerms, titleTerms, row.names = NULL)
str(trainBagOfWords)
head(trainBagOfWords)

trainSub = subset(fullData, select = c("requester_account_age_in_days_at_request", 
                                       "requester_days_since_first_post_on_raop_at_request", 
                                       "requester_number_of_comments_at_request",           
                                       "requester_number_of_comments_in_raop_at_request",   
                                       "requester_number_of_posts_at_request",              
                                       "requester_number_of_posts_on_raop_at_request",      
                                       "requester_number_of_subreddits_at_request", 
                                       "requester_upvotes_minus_downvotes_at_request",      
                                       "requester_upvotes_plus_downvotes_at_request", 
                                       "requester_received_pizza"))
newData = cbind(trainBagOfWords, trainSub)
colnames(newData)
str(newData)
unique(newData$requester_received_pizza)

newTrain = subset(newData, !is.na(requester_received_pizza))
str(newTrain)
newTest = subset(newData, is.na(requester_received_pizza))
str(newTest)
newTest$requester_received_pizza = NULL

logModel = glm(requester_received_pizza ~ ., data = newTrain, family = binomial)
summary(logModel)
predictions = predict(logModel, newdata = newTest, type = 'response')
head(predictions)
LogBinomialPred = as.numeric(predictions > 0.5)
unique(LogBinomialPred)

submission = data.frame(request_id = test$request_id, requester_received_pizza = binomialPred)
write.csv(submission, 'BagOfWordsLogModel.csv', row.names = FALSE)

#CART with bag of words
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

train(requester_received_pizza ~ ., data = newTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

pizzaCART = rpart(requester_received_pizza ~ ., data = newTrain, method = 'class', cp = 0.01)
predictions = predict(pizzaCART, newdata = newTest)

head(predictions)
binomialPred = as.numeric(predictions[, 2] > 0.5)
head(binomialPred)
unique(binomialPred)

submission = data.frame(request_id = test$request_id, requester_received_pizza = binomialPred)
write.csv(submission, 'BagOfWordsCARTModel.csv', row.names = FALSE)


#Use CV and get best tree
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.001)) 

tr = train(requester_received_pizza ~ ., data = newTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
names(tr)
bestTree = tr$finalModel
predictions = predict(tr, newdata = newTest)
head(predictions)
unique(predictions)

submission = data.frame(request_id = test$request_id, requester_received_pizza = binomialPred)
write.csv(submission, 'BagOfWordsCVModel.csv', row.names = FALSE)


#Random Forest Model with bag of words
library(randomForest)

optimumMTry = tuneRF(x = newTrain[, c(1:201)], 
                     y = newTrain[, c('requester_received_pizza')], ntreeTry = 500)

pizzaRF = randomForest(requester_received_pizza ~ ., data = newTrain, mtry = 14, nodesize = 25,
                       ntree = 500)
predictions = predict(pizzaRF, newdata = newTest, type = 'response')

head(predictions)
RFBinomialPred = as.numeric(as.logical(predictions))
head(RFBinomialPred)
unique(RFBinomialPred)

submission = data.frame(request_id = test$request_id, requester_received_pizza = binomialPred)
write.csv(submission, 'BagOfWordsRFModel.csv', row.names = FALSE)

#LogModel + RF Model Ensemble
predictions = (LogBinomialPred + RFBinomialPred) / 2
head(predictions)
unique(predictions)
binomialPred = as.numeric(predictions >= 0.5)
unique(binomialPred)


submission = data.frame(request_id = test$request_id, requester_received_pizza = binomialPred)
write.csv(submission, 'BagOfWordsRF+LogEnsemble.csv', row.names = FALSE)

#Log Model with only "giver_username_if_known" 
logModel1 = glm( requester_received_pizza ~ giver_username_if_known, data = fullData, family = binomial )
summary(logModel1)
predictions = predict(logModel1, newdata = test, type = 'response')

#Baseline Model - predict FALSE if giver_username_if_known == 'N/A' else TRUE
table(test$giver_username_if_known == "N/A")
predictions = ifelse(test$giver_username_if_known == "N/A", 0, 1)
table(predictions) #Should be opposite of previous table
submission = data.frame(request_id = test$request_id, requester_received_pizza = predictions)
write.csv(submission, 'baselineModel.csv', row.names = FALSE)
