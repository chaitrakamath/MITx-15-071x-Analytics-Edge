setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week5/Assignment')
emails = read.csv('emails.csv', stringsAsFactors = FALSE)

#1.1
str(emails)

#1.2
table(emails$spam)

#1.3
emails$text[[1]]
emails$text[[5]]

#1.5
max(nchar(emails$text))

#1.6
which.min(nchar(emails$text))

#2.1
library(tm)
emailCorpus = Corpus(VectorSource(emails$text))
emailCorpus = tm_map (emailCorpus, tolower)
emailCorpus = tm_map (emailCorpus, PlainTextDocument)
emailCorpus = tm_map (emailCorpus, removePunctuation)
emailCorpus = tm_map (emailCorpus, removeWords, stopwords('English'))
emailCorpus = tm_map (emailCorpus, stemDocument)
dtm = DocumentTermMatrix(emailCorpus)
dtm

#2.2
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

#2.3
emailSparse = as.data.frame(as.matrix(spdtm))
colnames(emailSparse) = make.names(colnames(emailSparse))

#2.4
which.max(colSums(emailSparse))

#2.5
emailSparse$spam = emails$spam
wordAgg = aggregate(emailSparse[, - emailSparse$spam], by = list(emailSparse$spam), FUN = sum)
sort(wordAgg[wordAgg$Group.1 == 0, ])

#2.6
sort(wordAgg[wordAgg$Group.1 == 1, ])

#3.1
emailSparse$spam = as.factor(emailSparse$spam)
library(caTools)
set.seed(123)
spl = sample.split(emailSparse$spam, SplitRatio = 0.7)
train = subset(emailSparse, spl == TRUE)
test = subset(emailSparse, spl == FALSE)

spamLog = glm(spam ~ ., data = train, family = binomial)
spamCART = rpart(spam ~ ., data = train, method = 'class')
spamRF = randomForest(spam ~ ., data = train)

logPred = predict(spamLog)
cartPred = predict(spamCART)
rfPred = predict(spamRF, type = 'prob')

length(which(logPred < 0.00001))
length(which(logPred > 0.9999))
length(which(logPred > 0.00001 & logPred < 0.9999))

#3.2
summary(spamLog)

#3.3
prp(spamCART)

#3.4
table(train$spam, logPred > 0.5)
logAccuracy = (3052 + 954) / nrow(train)
logAccuracy

#3.5
library(ROCR)
pred = prediction(logPred, train$spam)
perf = performance(pred, 'tpr', 'fpr')
performance(pred, 'auc')@y.values

#3.6
table(train$spam, cartPred[, 2] > 0.5)
cartAccuracy = (2885 + 894) / nrow(train)
cartAccuracy

#3.7
cartPrediction = prediction(cartPred[,2], train$spam)
performance(cartPrediction, 'auc')@y.values

#3.8
table(train$spam, rfPred[, 2] > 0.5)
rfAccuracy = (3016 + 918) / nrow(train)
rfAccuracy

#3.9
rfPrediction = prediction(rfPred[, 2], train$spam)
performance(rfPrediction, 'auc')@y.values

#4.1
logPred = predict(spamLog, newdata = test)
cartPred = predict(spamCART, newdata = test)
rfPred = predict(spamRF, newdata = test, type = 'prob')

table(test$spam, logPred > 0.5)
logAccuracy = (1258 + 376) / nrow(test)
logAccuracy

#4.2
logPrediction = prediction(logPred, test$spam)
performance(logPrediction, 'auc')@y.values

#4.3
table(test$spam, cartPred[, 2] > 0.5)
cartAccuracy = (1228 + 386) / nrow(test)
cartAccuracy

#4.4
cartPrediction = prediction(cartPred[, 2], test$spam)
performance(cartPrediction, 'auc')@y.values

#4.5
table(test$spam, rfPred[, 2] > 0.5)
rfAccuracy = (1292 + 389) / nrow(test)
rfAccuracy 

#4.6
rfPrediction = prediction(rfPred[, 2], test$spam)
performance(rfPrediction, 'auc')@y.values
