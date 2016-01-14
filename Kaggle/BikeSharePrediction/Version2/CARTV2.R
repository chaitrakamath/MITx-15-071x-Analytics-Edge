getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Kaggle/BikeSharePrediction/Version2")
getwd()

bikeTrain = read.csv('train.csv', stringsAsFactor = FALSE)
bikeTest = read.csv('test.csv',  stringsAsFactor = FALSE)

str(bikeTrain)
bikeTrain$datetime = strptime(bikeTrain$datetime, '%Y-%m-%d %H:%M:%S')
bikeTrain$Month = bikeTrain$datetime$mon
bikeTrain$Hour = bikeTrain$datetime$hour
bikeTrain$Weekday = bikeTrain$datetime$wday
sort(tapply(bikeTrain[, 'count'], bikeTrain[, 'Weekday'], mean))
bikeTrain$Sunday = as.numeric(bikeTrain$Weekday == 0)
bikeTrain$Friday = as.numeric(bikeTrain$Weekday == 5)
str(bikeTrain)

bikeTest$datetime = strptime(bikeTest$datetime, '%Y-%m-%d %H:%M:%S')
bikeTest$Month = bikeTest$datetime$mon
bikeTest$Hour = bikeTest$datetime$hour
bikeTest$Weekday = bikeTest$datetime$wday
bikeTest$Sunday = as.numeric(bikeTest$Weekday == 0)
bikeTest$Friday = as.numeric(bikeTest$Weekday == 5)
str(bikeTest)

library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (0:10)*0.001)

tr = train(count ~ season + holiday + workingday + weather + temp + atemp +
                   humidity + windspeed + Month + Hour + Sunday + Friday, data = bikeTrain, 
           method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

bestTree = tr$finalModel

treePredictions = predict(bestTree, newdata = bikeTest)
countPred = round(treePredictions, 0)
countPred[countPred < 0] = 0

submission = data.frame(datetime = bikeTest$datetime, count = countPred)
write.csv(submission, file = 'CARTModelV2.csv', row.names = FALSE)

