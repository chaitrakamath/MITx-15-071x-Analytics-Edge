earnings = read.csv('census.csv')
library(caTools)
str(earnings)
set.seed(2000)
spl = sample.split(earnings$over50k, SplitRatio = 0.6)
earningsTrain = subset(earnings, spl == TRUE)
earningsTest = subset(earnings, spl == FALSE)

logModel = glm (over50k ~ ., data = earningsTrain, family = binomial)
summary(logModel)

logPredict = predict(logModel, newdata = earningsTest, type = 'response')
table(earningsTest$over50k, logPredict > 0.5)
logAccuracy = (9051 + 1888) / nrow(earningsTest)
logAccuracy

table(earningsTest$over50k)
baselineAccuracy = 9713 / nrow(earningsTest)
baselineAccuracy
library(ROCR)
predROC = predict(logModel, earningsTest, type = 'response')
predROC
pred = prediction (predROC, earningsTest$over50k)
perf = performance (pred, 'tpr', 'fpr')
plot(perf)
as.numeric(performance(pred, 'auc')@y.values)

library(rpart)
library(rpart.plot)
CARTModel = rpart(over50k ~ . , data = earningsTrain, method = 'class')
prp(CARTModel)

predictCART = predict(CARTModel, newdata = earningsTest, type = 'class')
table(earningsTest$over50k, predictCART)
CARTAccuracy = (9243 + 1596) / nrow(earningsTest)
CARTAccuracy

library(ROCR)
predROC = predict(CARTModel, earningsTest)
predROC
pred = prediction (predROC[, 2], earningsTest$over50k)
perf = performance (pred, 'tpr', 'fpr')
plot(perf)
as.numeric(performance(pred, 'auc')@y.values)

library(randomForest)
set.seed(1)
trainSmall = earningsTrain[sample(nrow(earningsTrain), 2000), ]

set.seed(1)
earningsRandomForest = randomForest(over50k ~ ., data = trainSmall)
randomPredict = predict(earningsRandomForest, newdata = earningsTest, type = 'class')
head(randomPredict)
table(earningsTest$over50k, randomPredict)
randomAccuracy = (9585 + 1092) / nrow(earningsTest)
randomAccuracy

vu = varUsed(earningsRandomForest, count = TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(earningsRandomForest$forest$xlevels[vusorted$ix]))
varImpPlot(earningsRandomForest)

library(caret)
library(e1071)
set.seed(2)
numFolds = trainControl(method = 'cv', number = 10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data = earningsTrain, method = 'rpart', trControl = numFolds, tuneGrid = cartGrid)

earnings.trainCV = rpart(over50k ~ ., data = earningsTrain,method = 'class', cp = 0.002)
predictCV = predict(earnings.trainCV, newdata = earningsTest, type = 'class')
table(earningsTest$over50k, predictCV)
accuracy = (9178 + 1838) / nrow(earningsTest)
accuracy
prp(earnings.trainCV)

