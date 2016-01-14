setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week4/Assignment')
gerber = read.csv('gerber.csv')
str(gerber)

#1.1
nrow(gerber[gerber$voting == 1,]) / nrow(gerber)

#1.2
table(gerber$hawthorne) / nrow(gerber) * 100
table(gerber$civicduty) / nrow(gerber) * 100
table(gerber$neighbors) / nrow(gerber) * 100
table(gerber$self) / nrow(gerber) * 100

#1.3
logisticModel = glm(voting ~ hawthorne + civicduty + neighbors + self, data = gerber, family = binomial )
summary(logisticModel)
predictTest = predict(logisticModel, type = 'response')
table(gerber$voting, predictTest > 0.3)
accuracy = (134513 + 51966) / nrow(gerber)
accuracy

table(gerber$voting, predictTest > 0.5)
accuracy = (235388) / nrow(gerber)
accuracy

library(ROCR)
predictROC = prediction(predictTest, gerber$voting)
perf = performance (pred, 'tpr', 'fpr')
as.numeric(performance(pred, 'auc')@y.values)

CARTModel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTModel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(0.34 - 0.296638)

CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

logModel = glm(voting ~ sex + control, data = gerber, family = 'binomial')
summary(logModel)

possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logModel, newdata = possibilities, type = 'response')
abs(0.290456 - 0.2908065)

logModel2 = glm (voting ~ sex + control + sex:control, data = gerber, family = binomial)
summary(logModel2)
predict(logModel2, newdata=possibilities, type="response")
abs(0.290456 - 0.2904558)
