getwd() 
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Kaggle/BikeSharePrediction/Version2")
getwd()

bikeTrain = read.csv('train.csv', stringsAsFactor = FALSE)
bikeTest = read.csv('test.csv',  stringsAsFactor = FALSE)

str(bikeTrain)
bikeTrain$datetime = strptime(bikeTrain$datetime, '%Y-%m-%d %H:%M:%S')
bikeTrain$month = bikeTrain$datetime$mon
bikeTrain$hour = bikeTrain$datetime$hour
bikeTrain$weekday = bikeTrain$datetime$wday
sort(tapply(bikeTrain[, 'count'], bikeTrain[, 'weekday'], mean))
bikeTrain$Sunday = as.numeric(bikeTrain$weekday == 0)
bikeTrain$Friday = as.numeric(bikeTrain$weekday == 5)
bikeTrain$daySlot = 'night'
bikeTrain[bikeTrain$hour %in% c(6, 7, 8, 9, 10, 11), ]$daySlot = 'morning'     
bikeTrain[bikeTrain$hour %in% c(12, 13, 14, 15, 16), ]$daySlot = 'afternoon'  
bikeTrain[bikeTrain$hour %in% c(17, 18, 19, 20), ]$daySlot = 'evening'  
bikeTrain[bikeTrain$hour %in% c(21, 22, 23, 0, 1, 2, 3, 4, 5), ]$daySlot = 'night' 
bikeTrain$daySlot = as.factor(bikeTrain$daySlot)
bikeTrain$casual = NULL
bikeTrain$registered = NULL
str(bikeTrain)

bikeTest$datetime = strptime(bikeTest$datetime, '%Y-%m-%d %H:%M:%S')
bikeTest$month = bikeTest$datetime$mon
bikeTest$hour = bikeTest$datetime$hour
bikeTest$weekday = bikeTest$datetime$wday
bikeTest$Sunday = as.numeric(bikeTest$weekday == 0)
bikeTest$Friday = as.numeric(bikeTest$weekday == 5)
bikeTest$daySlot = 'night'
bikeTest[bikeTest$hour %in% c(6, 7, 8, 9, 10, 11), ]$daySlot = 'morning'     
bikeTest[bikeTest$hour %in% c(12, 13, 14, 15, 16), ]$daySlot = 'afternoon'  
bikeTest[bikeTest$hour %in% c(17, 18, 19, 20), ]$daySlot = 'evening'  
bikeTest[bikeTest$hour %in% c(21, 22, 23, 0, 1, 2, 3, 4, 5), ]$daySlot = 'night'
bikeTest$daySlot = as.factor(bikeTest$daySlot)
str(bikeTest)

library(randomForest)

optimumMTry = tuneRF(bikeTrain[, c('season', 'holiday', 'workingday', 'weather', 'temp', 
                                   'atemp', 'humidity', 'windspeed', 'month', 'hour', 'Sunday', 
                                   'Friday', 'daySlot')], bikeTrain[, 'count'])

rfModel = randomForest(count ~ season + holiday + workingday + weather + temp + atemp +
                               humidity + windspeed + month + hour + Sunday + Friday + daySlot, 
                       data = bikeTrain, mtry = 4, nodesize = 25, ntree = 200)
rfPredictions = predict(rfModel, newdata = bikeTest)

countPred = round(rfPredictions, 0)
countPred[countPred < 0] = 0

submission = data.frame(datetime = bikeTest$datetime, count = countPred)
write.csv(submission, 'RandomForestV3.csv', row.names = FALSE)
