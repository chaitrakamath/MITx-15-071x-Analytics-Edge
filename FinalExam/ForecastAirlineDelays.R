getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/FinalExam')
getwd()

#Q1
airlinesData = read.csv('AirlineDelay.csv')
str(airlinesData)
set.seed(15071)
spl = sample(nrow(airlinesData), 0.7 * nrow(airlinesData))
airlinesTrain = airlinesData[spl, ]
airlinesTest = airlinesData[-spl, ]

#Q3
linearFit = lm(TotalDelay ~ ., data = airlinesTrain)
summary(linearFit)

#Q5
cor(airlinesTrain$NumPrevFlights, airlinesTrain$PrevFlightGap)
cor(airlinesTrain$OriginAvgWind, airlinesTrain$OriginWindGust)

#Q9
absDiff = 1.571501 - (-5.418356)
absDiff2 = -4.506943 - (-5.418356)
absDiff
absDiff2

#Q10
predictions = predict(linearFit, newdata = airlinesTest)
SSE = sum((airlinesTest$TotalDelay - predictions) ^ 2)
SSE
SST = sum((airlinesTest$TotalDelay - mean(airlinesTrain$TotalDelay)) ^ 2)
SST
rSquared = 1 - (SSE / SST)
rSquared 

#Q12
airlinesData$DelayClass = factor(ifelse(airlinesData$TotalDelay == 0, 'No Delay', 
                                        ifelse(airlinesData$TotalDelay >=30, 'Major Delay', 
                                               'Minor Delay')))
nrow(airlinesData[airlinesData$DelayClass == 'No Delay', ])
nrow(airlinesData[airlinesData$DelayClass == 'Minor Delay', ])
nrow(airlinesData[airlinesData$DelayClass == 'Major Delay', ])

airlinesData$TotalDelay = NULL

library(caTools)
set.seed(15071)
spl = sample.split(airlinesData$DelayClass, SplitRatio = 0.7)
airlinesTrain = airlinesData[spl == TRUE, ]
airlinesTest = airlinesData[spl == FALSE, ]

#Q13
library(rpart)
library(rpart.plot)

airlinesTree = rpart(DelayClass ~ ., data = airlinesTrain, method = 'class')
prp(airlinesTree)

#Q15
delayTrainPred = predict(airlinesTree, type = 'class')
table(airlinesTrain$DelayClass, delayTrainPred)
trainAccuracy = (361 + 3094) / nrow(airlinesTrain)
trainAccuracy

#Q16
table(airlinesTrain$DelayClass)
baselineAccuracy = 3282 / nrow(airlinesTrain)
baselineAccuracy

#Q17
delayPred = predict(airlinesTree, newdata = airlinesTest, type = 'class')
table(airlinesTest$DelayClass, delayPred)
accuracy = (153 + 1301) / nrow(airlinesTest)
accuracy

#Q18
summary(linearFit)
