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
library(e1071)
library(rpart)
library(rpart.plot)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (0:10)*0.001)

tr = train(count ~ season + holiday + workingday + weather + temp + atemp +
              humidity + windspeed + Month + Hour + Weekday, data = bikeTrain, 
           method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

bestTree = tr$finalModel
prp(bestTree)

treePredictions = predict(bestTree, newdata = bikeTest)
countPred = as.integer(treePredictions)
countPred[countPred < 0] = 0

submission = data.frame(datetime = bikeTest$datetime, count = countPred)
write.csv(submission, file = 'CARTModel.csv', row.names = FALSE)
