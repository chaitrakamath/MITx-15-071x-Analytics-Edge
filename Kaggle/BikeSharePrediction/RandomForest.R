bikeTrain = read.csv('train.csv', stringsAsFactor = FALSE)
bikeTest = read.csv('test.csv',  stringsAsFactor = FALSE)

str(bikeTrain)
bikeTrain$datetime = strptime(bikeTrain$datetime, '%Y-%m-%d %H:%M:%S')
bikeTrain$Month = bikeTrain$datetime$mon
bikeTrain$Hour = bikeTrain$datetime$hour
bikeTrain$Weekday = bikeTrain$datetime$wday
str(bikeTrain)

bikeTest$datetime = strptime(bikeTest$datetime, '%Y-%m-%d %H:%M:%S')
bikeTest$Month = bikeTest$datetime$mon
bikeTest$Hour = bikeTest$datetime$hour
bikeTest$Weekday = bikeTest$datetime$wday
str(bikeTest)

library(caret)
library(randomForest)
library(e1071)

optimumMTry = tuneRF(bikeTrain[, c('season', 'holiday', 'workingday', 'weather', 'temp', 
                                   'atemp', 'humidity', 'windspeed', 'Month', 'Hour', 'Weekday')], 
                     bikeTrain[, 'count'])

rfModel = randomForest(count ~ season + holiday + workingday + weather + temp + atemp +
                              humidity + windspeed + Month +
                              Hour + Weekday, data = bikeTrain, mtry = 6, nodesize = 25, ntree = 200)
rfPredictions = predict(rfModel, newdata = bikeTest)
str(rfPredictions)
countPred = as.integer(rfPredictions)
countPred[countPred < 0] = 0

submission = data.frame(datetime = bikeTest$datetime, count = countPred)
write.csv(submission, file = 'RandomForest.csv', row.names = FALSE)
