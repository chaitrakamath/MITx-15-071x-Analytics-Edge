getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week6/Assignment/')
getwd()

#1.1
stocks = read.csv('StocksCluster.csv')
str(stocks)

#1.2
table(stocks$PositiveDec)
proportion = 6324 / (6324 + 5256)
proportion

#1.3
cor(stocks)
apply(cor(stocks), 2, sort)

#1.4
sort(colMeans(stocks))

#2.1
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
stocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
summary(stocksModel)
stocksTrainPredict = predict(stocksModel, type = 'response')
table(stocksTrain$PositiveDec, stocksTrainPredict > 0.5)
accuracy = (990 + 3640) / nrow(stocksTrain)
accuracy

#2.2
stocksPredict = predict(stocksModel, newdata = stocksTest, type = 'response')
table(stocksTest$PositiveDec, stocksPredict > 0.5)
testAccuracy = (417 + 1533) / nrow(stocksTest)
testAccuracy

#2.3
table(stocksTest$PositiveDec)
baselineAccuracy = 1897 / nrow(stocksTest)
baselineAccuracy

#3.1
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#3.2
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
colMeans(normTrain)
colMeans(normTest)

#3.4
k = 3
set.seed(144)
km = kmeans(normTrain, centers = k)
str(km)
table(km$cluster)

#3.5
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest)

#4.1
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#4.2
stockModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
stockModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
stockModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)

summary(stockModel1)
summary(stockModel2)
summary(stockModel3)

#4.3
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

predictTest1 = predict(stockModel1, newdata = stocksTest1, type = 'response')
table(stocksTest1$PositiveDec, predictTest1 > 0.5)
accuracy1 = (30 + 774) / nrow(stocksTest1)
accuracy1

predictTest2 = predict(stockModel2, newdata = stocksTest2, type = 'response')
table(stocksTest2$PositiveDec, predictTest2 > 0.5)
accuracy2 = (388 + 757) / nrow(stocksTest2)
accuracy2

predictTest3 = predict(stockModel3, newdata = stocksTest3, type = 'response')
table(stocksTest3$PositiveDec, predictTest3 > 0.5)
accuracy3 = (49 + 13) / nrow(stocksTest3)
accuracy3

#4.4
allPredictions = c(predictTest1, predictTest2, predictTest3)
allOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(allOutcomes, allPredictions > 0.5)
overallAccuracy = (467 + 1544) / length(allOutcomes)
overallAccuracy
