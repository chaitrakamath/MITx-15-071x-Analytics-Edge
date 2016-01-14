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
predictions = predict(linearModel, newdata = bikeTest)
countPred = as.integer(predictions)
countPred[countPred < 0] = 0

submission = data.frame(datetime = bikeTest$datetime, count = countPred)
write.csv(submission, file = 'SimpleLinearModel.csv', row.names = FALSE)
