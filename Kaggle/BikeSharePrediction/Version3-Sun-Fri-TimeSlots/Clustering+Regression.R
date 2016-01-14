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
bikeTrain$morning = as.numeric(bikeTrain$Hour %in% c(6, 7, 8, 9, 10, 11))
bikeTrain$afternoon = as.numeric(bikeTrain$Hour %in% c(12, 13, 14, 15, 16))
bikeTrain$evening = as.numeric(bikeTrain$Hour %in% c(17, 18, 19))
bikeTrain$night = as.numeric(bikeTrain$Hour %in% c(20, 21, 22, 23, 24, 25, 0, 1, 2, 3, 4, 5))
bikeTrain$Sunday = as.numeric(bikeTrain$Weekday == 0)
bikeTrain$Friday = as.numeric(bikeTrain$Weekday == 5)
str(bikeTrain)

bikeTest$datetime = strptime(bikeTest$datetime, '%Y-%m-%d %H:%M:%S')
bikeTest$Month = bikeTest$datetime$mon
bikeTest$Hour = bikeTest$datetime$hour
bikeTest$Weekday = bikeTest$datetime$wday
bikeTest$morning = as.numeric(bikeTest$Hour %in% c(6, 7, 8, 9, 10, 11))
bikeTest$afternoon = as.numeric(bikeTest$Hour %in% c(12, 13, 14, 15, 16))
bikeTest$evening = as.numeric(bikeTest$Hour %in% c(17, 18, 19))
bikeTest$night = as.numeric(bikeTest$Hour %in% c(20, 21, 22, 23, 24, 25, 0, 1, 2, 3, 4, 5))
bikeTest$Sunday = as.numeric(bikeTest$Weekday == 0)
bikeTest$Friday = as.numeric(bikeTest$Weekday == 5)
str(bikeTest)

bikeTrain$datetime = NULL
bikeTrain$casual = NULL
bikeTrain$registered = NULL
bikeTrain$Hour = NULL
str(bikeTrain)
bikeTrain = bikeTrain[c(9, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17)]
str(bikeTrain)
bikeTrain$morning = as.numeric(bikeTrain$morning)
bikeTrain$afternoon = as.numeric(bikeTrain$afternoon)
bikeTrain$evening = as.numeric(bikeTrain$evening)
bikeTrain$night = as.numeric(bikeTrain$night)
str(bikeTrain)

str(bikeTest)
bikeTest$datetime = NULL
bikeTest$night = as.numeric(bikeTest$night)
str(bikeTest)

bikeTrainMat = as.matrix(bikeTrain)
bikeTrainVector = as.vector(bikeTrainMat)

k = 5
set.seed(1)
kmeansModel = kmeans(bikeTrainVector, centers = k, iter.max = 1000)
str(kmeansModel)


