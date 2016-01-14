getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week3/Assignment')
getwd()

#1.1
loans = read.csv('loans.csv')
str(loans)
summary(loans)
table(loans$not.fully.paid)
prop = 1533 / (1533 + 8045)

#1.4
loansImp = read.csv('loans_imputed.csv')

#2.1
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loansTrain = subset(loans, split == TRUE)
loansTest = subset(loans, split == FALSE)

loansModel = glm (not.fully.paid ~ ., data = loansTrain, family = 'binomial')
summary(loansModel)

#2.2
-9.406e-03 * 10
exp(700 * -9.406e-03) / exp(710 * -9.406e-03)

#2.3
predictedRisk = predict(loansModel, newdata = loansTest, type = 'response')
table(loansTest$not.fully.paid, predictedRisk > 0.5)
accuracy = (2400 + 3) / (2400 + 13 + 457 + 3)
table(loansTest$not.fully.paid)
accuracy = 2413 / (2413 + 460)

#2.4
library(ROCR)
ROCRPred = prediction (predictedRisk, loansTest$not.fully.paid)
ROCRPerf = performance(ROCRPred, 'tpr', 'fpr')
as.numeric(performance(ROCRPred, "auc")@y.values)

#3.1
model2 = glm(not.fully.paid ~ int.rate, data = loansTrain, family = binomial)
summary(model2)

#3.2
loanPredict = predict(model2, newdata = loansTest, type = 'response')
max(loanPredict)
table(loansTest$not.fully.paid, loanPredict < 0.5)


#3.3
library(ROCR)
ROCRPred = prediction (loanPredict, loansTest$not.fully.paid)
ROCRPerf = performance(ROCRPred, 'tpr', 'fpr')
as.numeric(performance(ROCRPred, "auc")@y.values)


#5.1
loansTest$profit = exp(loansTest$int.rate*3) - 1
loansTest$profit[loansTest$not.fully.paid == 1] = -1
max(loansTest$profit) * 10


#6.1
highInterest = subset(loansTest, int.rate >= -.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)
460 + 2413
str(loansTest)

highInterest$predicted.risk = predictedRisk
names(highInterest)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk < cutoff)
str(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
