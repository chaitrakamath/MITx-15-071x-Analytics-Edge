library(caret)
install.packages('pROC')
library(pROC)
library(gbm)

train = nyTrain
str(train)
train$Popular = as.factor(train$Popular)
str(train)


fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:100)*0.001) 
train(Popular ~ ., data = nyTrain, method = 'rpart', trControl = fitControl, tuneGrid = cartGrid)




objControl = trainControl(method = 'repeatedcv', number = 10, repeats = 10, 
                          summaryFunction = twoClassSummary, classProbs = TRUE)

gbmGrid = expand.grid(interaction.depth =  c(1, 5, 9), n.trees = (1:30)*50,
                      shrinkage = 0.1)

gbmModel = train (Popular ~ . , data = train, method = 'gbm', trControl = objControl,
                tuneGrid = gbmGrid, verbose = TRUE)


test = nyTest
str(test)
test$Popular = NULL
str(test)

gbmPredict = predict.gbm(gbmModel$finalModel, newdata = test[, - test$UniqueID],
                         type = 'response', n.trees = 650)
head(gbmPredict)

mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = gbmPredict)
write.csv(mySubmission, 'simpleGBM-NoTextAnalytics.csv', row.names = FALSE)
