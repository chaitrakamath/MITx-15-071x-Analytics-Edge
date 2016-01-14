getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week2/Assignment/Part3')
getwd()

fluTrain = read.csv('FluTrain.csv')
str(fluTrain)

#1.1
fluTrain[which.max(fluTrain$ILI), ]
fluTrain[with(fluTrain, order(-ILI)),] #alternative
fluTrain[which.max(fluTrain$Queries),]

#1.2
hist(fluTrain$ILI)

#1.3
plot(log(fluTrain$ILI) ~ fluTrain$Queries, xlabel = 'Queries', ylabel = 'Log of ILI', main = 'ILI vs Queries')

#2.2
fluTrend1 = lm(log(ILI) ~ Queries, data = fluTrain)
summary(fluTrend1)

#3.1
fluTest = read.csv('FluTest.csv')
fluPred = exp(predict(fluTrend1, newdata = fluTest))
estimatedILI = fluPred[which(fluTest$Week == '2012-03-11 - 2012-03-17')]

#3.2
obsILI = fluTest[which(fluTest$Week == '2012-03-11 - 2012-03-17'), 'ILI']
(obsILI - estimatedILI) / obsILI

#3.3
SSE1 = sum((fluTest$ILI - fluPred) ^ 2)
rmseTest1 = sqrt(SSE / nrow(fluTest))

#4.1
install.packages('zoo')
library(zoo)
ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad = TRUE)
fluTrain$ILILag2 = coredata(ILILag2)
summary(fluTrain)

#4.2
plot(fluTrain$ILILag2 ~ fluTrain$ILI)

#4.3
fluTrend2 = lm(log(ILI) ~ (Queries + log(ILILag2)), data = fluTrain)
summary(fluTrend2)

#5.1
ILILag2 = lag(zoo(fluTest$ILI), -2, na.pad = TRUE)
fluTest$ILILag2 = coredata(ILILag2)
summary(fluTest)

#5.3
fluTest[1, 'ILILag2'] = fluTrain[nrow(fluTrain) - 1, 'ILI']
fluTest[2, 'ILILag2'] = fluTrain[nrow(fluTrain), 'ILI']
head(fluTest)

#5.4
fluPred2 = exp(predict(fluTrend2, newdata = fluTest))
SSE2 = sum((fluPred2 - fluTest$ILI) ^ 2)
rmseTest2 = sqrt (SSE / nrow(fluTest))
