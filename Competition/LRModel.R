logModel = glm(Popular ~ ., data = nyTrain, family = binomial)
summary(logModel)
logPredict = predict(logModel, newdata = nyTest, type = 'response')
head(logPredict)
