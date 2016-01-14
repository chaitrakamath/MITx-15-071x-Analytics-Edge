setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week4/Lecture')
stevens = read.csv('stevens.csv')
str(stevens)
summary(stevens)
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, spl == TRUE)
test = subset(stevens, spl == FALSE)
install.packages('rpart')
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
stevens.tree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                             Unconst, data = train, method = 'class', minbucket = 25) 
prp(stevens.tree)
predictCART = predict(stevens.tree, newdata = test, type = 'class')
table(test$Reverse, predictCART)
accuracy = (41 + 71) / (41 + 36 + 22 + 71)
library(ROCR)
predictROC = predict(stevens.tree, newdata = test)
predictROC
pred = prediction (predictROC[, 2], test$Reverse)
perf = performance (pred, 'tpr', 'fpr')
plot(perf)

#Quiz
as.numeric(performance(pred, 'auc')@y.values)
stevens.tree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                             Unconst, data = train, method = 'class', minbucket = 5) 
prp(stevens.tree2)
stevens.tree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                              Unconst, data = train, method = 'class', minbucket = 100) 
prp(stevens.tree3)
#QuizEnd

install.packages('randomForest')
library(randomForest)
stevens.forest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data = train, nodesize = 25, ntree = 200)
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
stevens.forest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                              data = train, nodesize = 25, ntree = 200)
predictForest = predict(stevens.forest, newdata = test)
table(test$Reverse, predictForest)
accuracy = (40 + 74) / (40 + 37 + 19 + 74) 
accuracy

#Quiz
set.seed(100)
stevens.forest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                              data = train, nodesize = 25, ntree = 200)
predictForest2 = predict(stevens.forest2, newdata = test)
table(test$Reverse, predictForest2)
accuracy = (43 + 74) / (43 + 34 + 19 + 74)
accuracy

set.seed(200)
stevens.forest3 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                               data = train, nodesize = 25, ntree = 200)
predictForest3 = predict(stevens.forest3, newdata = test)
table(test$Reverse, predictForest3)
accuracy = (44 + 76) / (43 + 33 + 17 + 76)
accuracy
##QuizEnd

install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)

numFolds = trainControl(method = 'cv', number = 10)
cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))
cpGrid
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, 
      method = 'rpart', trControl = numFolds, tuneGrid = cpGrid)

stevens.treeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train,
                       method = 'class', cp = 0.19)
predictCV = predict(stevens.treeCV, newdata = test, type = 'class')
table(test$Reverse, predictCV)
accuracy = (59 + 64) / (59 + 18 + 29 + 64)
accuracy

#Quiz
prp(stevens.treeCV,main = 'quiz')
#QuizEnd