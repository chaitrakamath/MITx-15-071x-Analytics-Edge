setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week4/Lecture')
claims = read.csv('ClaimsData.csv')
str(claims)
summary(claims)
table(claims$bucket2009) / nrow(claims)
library(caTools)
set.seed(88)
spl = sample.split(claims$bucket2009, SplitRatio = 0.6)
claimsTrain = subset(claims, spl == TRUE)
claimsTest = subset(claims, spl == FALSE)

#Quiz
mean(claimsTrain$age)
nrow(claimsTrain[claimsTrain$diabetes == 1, ]) / nrow(claimsTrain)
#QuizEnd

table(claimsTest$bucket2009, claimsTest$bucket2008)
accuracy = (110138 + 10721 + 2774 + 1539 + 104) / nrow(claimsTest)
accuracy
penaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow = TRUE, nrow = 5 )
penaltyMatrix
as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * penaltyMatrix
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * penaltyMatrix) / nrow(claimsTest)

#Quiz
prediction = rep_len(1, nrow(claimsTest))
table(claimsTest$bucket2009, prediction)
accuracy = 122978 / nrow(claimsTest)
accuracy
penaltyMatrix1 = matrix(c(0, 2, 4, 6, 8),byrow = TRUE, nrow = 5 )
penaltyMatrix1
as.matrix(table(claimsTest$bucket2009, prediction)) * penaltyMatrix1
penaltyError = sum(as.matrix(table(claimsTest$bucket2009, prediction)) * penaltyMatrix1) / nrow(claimsTest)
penaltyError
#QuizEnd

claimsTree = rpart(bucket2009 ~ . - reimbursement2009, data = claimsTrain, method = 'class', cp = 0.00005)
prp(claimsTree)
predictTest = predict(claimsTree, newdata = claimsTest, type = 'class')
table(claimsTest$bucket2009, predictTest)
accuracy = (114141 + 16102 + 118 + 201 + 0) / nrow(claimsTest) 
accuracy
penaltyError = sum(as.matrix(table(claimsTest$bucket2009, predictTest)) * penaltyMatrix) / nrow(claimsTest)
penaltyError


claimsTree = rpart(bucket2009 ~ . - reimbursement2009, data = claimsTrain, method = 'class', cp = 0.00005, parms = list(loss = penaltyMatrix))
predictTest = predict(claimsTree, newdata = claimsTest, type = 'class')
table(claimsTest$bucket2009, predictTest)
accuracy = (114141 + 16102 + 118 + 201 + 0) / nrow(claimsTest) 
accuracy
penaltyError = sum(as.matrix(table(claimsTest$bucket2009, predictTest)) * penaltyMatrix) / nrow(claimsTest)
penaltyError