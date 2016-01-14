library(e1071)
tunedParams = tune.svm(Popular ~ ., data = nyTrain, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tunedParams)

train = nyTrain
str(train)
train$Popular = as.factor(train$Popular)
str(train)
svmModel = svm (Popular ~ ., data = train, kernel = 'linear', gamma = 0.001, cost = 10, probability = TRUE)
summary(svmModel)

test = nyTest
str(test)
test$Popular = NULL
str(test)
svmPredict = predict(svmModel, newdata = test, probability = TRUE)


length(svmPredict)
head(svmPredict)
svmPredictions = attr(svmPredict, 'prob')[ , 1]
str(svmPredictions)

mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = svmPredictions)
write.csv(mySubmission, 'SVMModel.csv', row.names = FALSE)
