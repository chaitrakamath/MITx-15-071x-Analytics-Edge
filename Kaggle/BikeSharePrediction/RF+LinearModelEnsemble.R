getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Kaggle/BikeSharePrediction')
getwd()

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

linearModel = lm(count ~ season + holiday + workingday + weather + temp + atemp +
                         humidity + windspeed + Month +
                         Hour + Weekday, data = bikeTrain)
summary(linearModel)
linearPredictions = predict(linearModel, newdata = bikeTest)


rfModel = randomForest(count ~ season + holiday + workingday + weather + temp + atemp +
                               humidity + windspeed + Month +
                               Hour + Weekday, data = bikeTrain, mtry = 6, nodesize = 25, ntree = 200)
rfPredictions = predict(rfModel, newdata = bikeTest)


predictions = (rfPredictions + linearPredictions) / 2
predictions[predictions < 0] = 0
countPred = as.integer(predictions)

submission = data.frame(datetime = bikeTest$datetime, count = countPred)
write.csv(submission, file = 'RF+LinearModelEnsemble.csv', row.names = FALSE)
