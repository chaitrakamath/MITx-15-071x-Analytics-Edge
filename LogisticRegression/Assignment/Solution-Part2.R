getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week3/Assignment')
getwd()

#1.1
paroles = read.csv('parole.csv')
str(paroles)
summary(paroles)

#1.2
length(paroles[paroles$violator == 1, paroles$violator])

#2.2
summary(paroles)
paroles$state = as.factor(paroles$state)
paroles$crime = as.factor(paroles$crime)
summary(paroles)

#3.1
set.seed(144)
library(caTools)
split = sample.split(paroles$violator, SplitRatio = 0.7)
paroleTrain = subset(paroles, split == TRUE)
paroleTest = subset(paroles, split == FALSE)

#4.1
paroleModel = glm (violator ~ ., data = paroleTrain, family = binomial)
summary(paroleModel)

#4.3
test2 = data.frame(t(c(1,1,50,1,3,12,0,2,1)))
names(test2) = names(paroleTest)
test2$crime = as.factor(test2$crime)
test2$state = as.factor(test2$state)
prob.y1 = predict(paroleModel, newdata = test2, type = 'response')
prob.y0 = 1 - prob.y1
prob.y1 / prob.y0

#5.1
predictTest = predict(paroleModel, newdata = paroleTest, type = 'response')
max(predictTest)

#5.2
table(paroleTest$violator, predictTest > 0.5)

sensitivity = 12 / (11 + 12)
sensitivity
specificity = 167 / (167 + 12)
specificity
accuracy = (167 + 12) / (167 + 12 + 11 + 12)
accuracy

#5.3
table(paroleTest$violator)
accuracy = 179 / (179 + 23)

#5.6
library(ROCR)
ROCRPred = prediction (predictTest, paroleTest$violator)
ROCRPerf = performance(ROCRPred, 'tpr', 'fpr')
as.numeric(performance(ROCRPred, "auc")@y.values)
