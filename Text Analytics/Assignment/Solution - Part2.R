setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week5/Assignment')
trials = read.csv('clinical_trial.csv', stringsAsFactors = FALSE)

#1.1
max(nchar(trials$abstract))

#1.2
table(nchar(trials$abstract) == 0)

#1.3
trials[which.min(nchar(trials$title)), ]

#2.1
library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle[[1]]
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle[[1]]
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle[[1]]
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle[[1]]
corpusTitle = tm_map(corpusTitle, removeWords, stopwords('English'))
corpusTitle[[1]]
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusTitle[[1]]

corpusAbstract[[5]]
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract[[5]]
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract[[5]]
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract[[5]]
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords('English'))
corpusAbstract[[5]]
corpusAbstract = tm_map(corpusAbstract, stemDocument)
corpusAbstract[[5]]

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
ncol (dtmTitle)
ncol(dtmAbstract)

#2.3
nrow(dtmAbstract)
max(colSums(dtmAbstract))
which.max(colSums(dtmAbstract))

#3.1
colnames(dtmTitle) = paste0('T', colnames(dtmTitle))
colnames(dtmAbstract) = paste0('A', colnames(dtmAbstract))

#3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)

#3.3
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(test$trial)
baselineAccuracy = 313 / nrow(test)
baselineAccuracy

#3.4
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data = train, method = 'class')
prp(trialCART)

#3.5
trainPred = predict(trialCART)
head(trainPred)
max(trainPred[, 2])

#3.7
trainPred = predict(trialCART, type = 'class')
table(train$trial, trainPred)
trainAccuracy = (631 + 441) / nrow(train)
trainAccuracy

sensitivity = 441 / (131 + 441)
sensitivity
specificity = 631 / (631 + 99)
specificity

#4.1
pred = predict(trialCART, newdata = test, type = 'class')
table(test$trial, pred)
accuracy = (261 + 162) / nrow(test)
accuracy

#4.2
library(ROCR)
pred.prob = predict(trialCART, newdata = test)
nrow(pred.prob)
nrow(test)
predROCR = prediction(pred.prob[, 2], test$trial)
perf = performance(predROCR, 'tpr', 'fpr')
plot(perf, colorize = TRUE)
performance(predROCR, 'auc')@y.values
