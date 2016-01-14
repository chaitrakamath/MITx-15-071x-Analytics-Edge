setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week4/Lecture')
boston = read.csv('boston.csv')
str(boston)
plot(boston$LAT ~ boston$LON)
points(boston$LAT[boston$CHAS == 1] ~ boston$LON[boston$CHAS == 1], col = 'blue', pch = 19)
points(boston$LAT[boston$TRACT == 3531]~boston$LON[boston$TRACT == 3531], col = 'red', pch = 19)
summary(boston$NOX)
points(boston$LAT[boston$NOX >= 0.55] ~ boston$LON[boston$NOX >= 0.55], col = 'green', pch = 19)
plot(boston$LAT ~ boston$LON)
summary(boston$MEDV)
points(boston$LAT[boston$MEDV > 21.2] ~ boston$LON [boston$MEDV > 21.2], col = 'red', pch = 19)
plot(boston$MEDV ~ boston$LON)
plot(boston$MEDV ~ boston$LAT)
latlonlm = lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)
plot(boston$LAT ~ boston$LON)
points(boston$LAT[boston$MEDV > 21.2] ~ boston$LON [boston$MEDV > 21.2], col = 'red', pch = 19)
points(boston$LAT[latlonlm$fitted.values > 21.2] ~ boston$LON[latlonlm$fitted.values > 21.2], col = 'blue', pch = '$')

library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)
fittedValues = predict(latlontree)
plot(boston$LAT ~ boston$LON)
points(boston$LAT[fittedValues > 21.2] ~ boston$LON[fittedValues > 21.2], pch = '$', col = 'blue')

latlontree = rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
plot(latlontree)
text(latlontree)

library(caTools)
set.seed(123)
spl = sample.split(boston$MEDV, SplitRatio = 0.7)
bostonTrain = subset(boston, spl == TRUE)
bostonTest = subset(boston, spl == FALSE)
linreg = lm (MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = bostonTrain)
summary(linreg)
predict.linreg = predict (linreg, newdata = bostonTest)
sse = sum((predict.linreg - bostonTest$MEDV) ^ 2)
sse

tree = rpart (MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = bostonTrain)
prp(tree)
tree.pred = predict(tree, newdata = bostonTest)
tree.sse = sum((tree.pred - bostonTest$MEDV) ^ 2)
tree.sse

library(caret)
library(e1071)
tr.control = trainControl(method = 'cv', number = 10)
cp.grid = expand.grid(.cp = (0:10) * 0.001)
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + CHAS + PTRATIO, 
           data = bostonTrain, method = 'rpart', trControl = tr.control, tuneGrid = cp.grid)
tr

best.tree = tr$finalModel
prp(best.tree)
best.predict = predict(best.tree, newdata = bostonTest)
best.sse = sum((best.predict - bostonTest$MEDV) ^ 2)
best.sse
sse
