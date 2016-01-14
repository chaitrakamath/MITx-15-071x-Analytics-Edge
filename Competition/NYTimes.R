getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Competition")
getwd()

nyTrain = read.csv('NYTimesBlogTrain.csv', stringsAsFactors = FALSE)
nyTest = read.csv('NYTimesBlogTest.csv', stringsAsFactors = FALSE)
str(nyTrain)

#Simple Log Model
logModel = glm(Popular ~ NewsDesk + SectionName + SubsectionName  + WordCount, data = nyTrain, family = binomial)
summary(logModel)
logPredict = predict(logModel, newdata = nyTest, type = 'response')

#Submission1 - Result: 0.9052
MySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = logPredict)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)


#Adding days and time as independent variables to log model
nyTrain$PubDate = strptime(nyTrain$PubDate, '%Y-%m-%d %H:%M:%S')
nyTest$PubDate = strptime(nyTest$PubDate, '%Y-%m-%d %H:%M:%S')
str(nyTrain)
str(nyTest)

nyTrain$Weekday = nyTrain$PubDate$wday
nyTest$Weekday = nyTest$PubDate$wday

logModel2 = glm(Popular ~ NewsDesk + SectionName + SubsectionName  + WordCount + Weekday, data = nyTrain, family = binomial)
logPredict2 = predict(logModel2, newdata = nyTest, type = 'response')

#Submission2 - Result: No improvement over Submission 1
MySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = logPredict2)
write.csv(MySubmission, "SubmissionSimpleLog2.csv", row.names=FALSE)

#Simple Random Forest Model 
library(randomForest)
nyTrain = read.csv('NYTimesBlogTrain.csv', stringsAsFactors = FALSE)
nyTest = read.csv('NYTimesBlogTest.csv', stringsAsFactors = FALSE)
nyTrain$PubDate = strptime(nyTrain$PubDate, '%Y-%m-%d %H:%M:%S')
nyTest$PubDate = strptime(nyTest$PubDate, '%Y-%m-%d %H:%M:%S')
nyTrain$Weekday = nyTrain$PubDate$wday
nyTest$Weekday = nyTest$PubDate$wday
set.seed(100)
simpleCARTModel = rpart(Popular ~ NewsDesk + SectionName + SubsectionName + WordCount + Weekday, 
                        data = nyTrain, method = 'class')
simpleCARTPredictions = predict(simpleCARTModel, newdata = nyTest)
head(simpleCARTPredictions)
head(simpleCARTPredictions[, 2])

#Submission 4 - Simple Random Forest model w/o bag of words - no improvement
mySubmission = data.frame(UniqueID = nyTest$UniqueID, 
                          Probability1 = simpleCARTPredictions[ , 2])
write.csv(mySubmission, "SubmissionSimpleLog4.csv", row.names=FALSE)

#Create bag of words for headlines of merged data set
library(tm)
library(SnowballC)
nyTest$Popular = NA
#--merge train and test data sets into one
nyMerged = rbind(nyTrain, nyTest)
str(nyMerged)
#--create dtm for headlines
headlinesCorpus = Corpus(VectorSource(nyMerged$Headline))
headlinesCorpus[[1]]
headlinesCorpus = tm_map(headlinesCorpus, tolower)
headlinesCorpus[[1]]
headlinesCorpus = tm_map(headlinesCorpus,PlainTextDocument)
headlinesCorpus = tm_map(headlinesCorpus,removePunctuation)
headlinesCorpus[[1]]
headlinesCorpus = tm_map(headlinesCorpus,removeWords, stopwords('English'))
headlinesCorpus[[1]]
headlinesCorpus = tm_map(headlinesCorpus,stemDocument)
headlinesCorpus[[1]]
headlinesDTM = DocumentTermMatrix(headlinesCorpus)
headlinesDTM
headlinesSparse = removeSparseTerms(headlinesDTM, 0.99)
headlinesSparse
headlinesSparse = as.data.frame(as.matrix(headlinesSparse))
str(headlinesSparse)
#--add other independent variables to the model and also dependent variable
nyData = cbind(nyMerged, headlinesSparse)
str(nyData)
nyData$Abstract = NULL
nyData$Headline = NULL
nyData$Snippet = NULL
nyData$PubDate = NULL
colnames(nyData) = make.names(colnames(nyData))
str(nyData)


nyTrain = subset(nyData, !is.na(nyData$Popular))
nyTest = subset(nyData, is.na(nyData$Popular))
str(nyTrain)
str(nyTest)
#--use CART and logistic regression to build model
library(rpart)
library(rpart.plot)
cartModel = rpart(Popular ~ ., data = nyTrain, method = 'class')
prp(cartModel)
cartPredict = predict(cartModel, newdata = nyTest)
head(cartPredict)

#Submission 3 - bag of words for headlines and CART model (No improvement over regular log model)
MySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = cartPredict[ , 1])
write.csv(MySubmission, "SubmissionSimpleLog3.csv", row.names=FALSE)

#use Random Forest to build model
library(randomForest)
nyTrain$Popular = as.factor(nyTrain$Popular)
str(nyTrain)
nyTrain$UniqueID = NULL
nyTrain$NewsDesk[nyTrain$NewsDesk==""] = 'Other'
nyTrain$SectionName[nyTrain$SectionName == ""] = 'Other'
nyTrain$NewsDesk = as.factor(nyTrain$NewsDesk)
nyTrain$SectionName = as.factor(nyTrain$SectionName)
nyTrain$SubsectionName = as.factor(nyTrain$SubsectionName)
str(nyTrain)
rfModel = randomForest(Popular ~ ., data = nyTrain, ntrees = 200, nodesize = 25)

str(nyTest)
nyTest$UniqueID = NULL
#nyTest$Popular = NULL
nyTest$NewsDesk[nyTest$NewsDesk == ""] = 'Other'
nyTest$SectionName[nyTest$SectionName == ""] = 'Other'
nyTest$SubsectionName[nyTest$SubsectionName == ""] = 'Other'
nyTest$NewsDesk = as.factor(nyTest$NewsDesk)
nyTest$SectionName = as.factor(nyTest$SectionName)
nyTest$SubsectionName = as.factor(nyTest$SubsectionName)
str(nyTest)

rfPredict = predict(rfModel, newdata = nyTest)
colnames(nyTest)
colnames(nyTrain)



#Gradient Boosted Model
