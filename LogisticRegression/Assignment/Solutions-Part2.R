letters = read.csv('letters_ABPR.csv')
letters$isB = as.factor(letters$letter == 'B')
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain = subset(letters, spl == TRUE)
lettersTest = subset(letters, spl == FALSE)
table(lettersTest$isB)
baselineAccuracy = 1175 / (1175 + 383)
baselineAccuracy

library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data = lettersTrain, method = 'class')
prp(CARTb)

predictCARTb = predict(CARTb, newdata = lettersTest, type = 'class')
table(lettersTest$isB, predictCARTb)
accuracy = (1118 + 340) / nrow(lettersTest)
accuracy


library(randomForest)
set.seed(1000)
bRandomForests = randomForest(isB ~ . - letter, data = lettersTrain)
predict.forest = predict(bRandomForests, newdata = lettersTest, type = 'class')
table(lettersTest$isB, predict.forest)
forestAccuracy = (1165 + 374) / nrow(lettersTest)
forestAccuracy

letters$letter = as.factor(letters$letter)
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
lettersTrain = subset(letters, spl == TRUE)
lettersTest = subset(letters, spl == FALSE)
table(lettersTest$letter)
baselineAccuracy = 401 / nrow(lettersTest)
baselineAccuracy

library (rpart)
CARTModel = rpart(letter ~ . - isB, data = lettersTrain, method = 'class')
predict.CARTModel = predict(CARTModel, newdata = lettersTest, type = 'class')
table(lettersTest$letter, predict.CARTModel)
accuracy = (348 + 318 + 363 + 340) / nrow(lettersTest)
accuracy

library(randomForest)
set.seed(1000)
lettersRandomForest = randomForest(letter ~ . - isB, data = lettersTrain)
predict.random = predict(lettersRandomForest, newdata = lettersTest, type = 'class')
table(lettersTest$letter, predict.random)
random.accuracy = (390 + 380 + 393 + 364) / nrow(lettersTest)
random.accuracy
