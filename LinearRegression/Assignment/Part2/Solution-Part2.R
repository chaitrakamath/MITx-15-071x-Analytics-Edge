getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week2/Assignment/Part2')
getwd()
pisaTrain = read.csv('pisa2009train.csv')
pisaTest = read.csv('pisa2009test.csv')

#1.1
str(pisaTrain)
summary(pisaTrain)

str(pisaTest)
summary(pisaTest)

#1.2
tapply(pisaTrain$readingScore, pisaTrain$male == 1, mean, na.rm = TRUE)

#1.3
summary(pisaTrain)

#1.4
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

str(pisaTrain)
str(pisaTest)

#3.1
pisaTrain$raceeth = relevel(pisaTrain$raceeth, 'White')
pisaTest$raceeth = relevel (pisaTest$raceeth, 'White')

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

#3.2
mse = mean(residuals(lmScore) ^ 2)
rmse = sqrt(mse)

#4.1
predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)

#4.2
SSE = sum((predTest - pisaTest$readingScore) ^ 2)
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore) ^ 2)
R2 = 1 - (SSE / SST)
RMSE = sqrt(SSE / nrow(pisaTest))

#4.3
mean(pisaTrain$readingScore)
