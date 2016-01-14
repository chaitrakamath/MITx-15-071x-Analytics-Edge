#First run all of the steps from TextProcessingSteps.R file to get nyTrain and nyTest datasets
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

tr.control = trainControl(method = 'cv', number = 10)
cp.grid = expand.grid(.cp = (0:10) * 0.001)

tr = train(Popular ~ ., data = nyTrain, method = 'rpart', trControl = tr.control, 
           tuneGrid = cp.grid)
tr

best.tree = tr$finalModel
prp(best.tree)

best.tree.pred = predict(tr, newdata = nyTest)
head(best.tree.pred)

mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = best.tree.pred)
write.csv(mySubmission, 'TuneCARTModel.csv', row.names = FALSE)
