library(randomForest)
set.seed(1)
selectedMTry = tuneRF(nyTrain[ , - nyTrain$Popular], y = nyTrain[, 'Popular'])

set.seed(1)
rfModel = randomForest(Popular ~ ., data = nyTrain, ntree = 1000, nodesize = 5, mytry = 45)
rfPredict = predict(rfModel, newdata = nyTest)
head(rfPredict)
#This file gave a score of 0.91155
mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = rfPredict)
write.csv(mySubmission, 'TunedRF.csv', row.names = FALSE)

rfModel = randomForest(Popular ~ ., data = nyTrain, ntree = 3000, nodesize = 5, mytry = 45)
rfPredict = predict(rfModel, newdata = nyTest)
head(rfPredict)
mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = rfPredict)
write.csv(mySubmission, 'TunedRFv2.csv', row.names = FALSE)


#Simple RF Model - without Text Analytics
library(randomForest)
set.seed(1)
selectedMTry = tuneRF(nyTrain[ , c('NewsDesk', 'SectionName','SubsectionName', 'WordCount')],
                      y = nyTrain[, 'Popular'])

set.seed(1)
rfModel = randomForest(Popular ~ NewsDesk + SectionName + SubsectionName  + WordCount, 
                       data = nyTrain, ntree = 1000, nodesize = 5, mytry = 2)
rfPredict_NoTextAnalytics = predict(rfModel, newdata = nyTest)
head(rfPredict_NoTextAnalytics)


mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = rfPredict_NoTextAnalytics)
write.csv(mySubmission, 'TunedRF-NoTextAnalytics.csv', row.names = FALSE)


