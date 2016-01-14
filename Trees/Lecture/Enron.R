setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week5/Lecture')
emails = read.csv('energy_bids.csv', stringsAsFactors = FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1])
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]
table(emails$responsive)

library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus = tm_map (corpus, tolower)
corpus = tm_map (corpus, PlainTextDocument)
corpus = tm_map (corpus, removePunctuation)
corpus = tm_map (corpus, removeWords, stopwords('English'))
corpus = tm_map (corpus, stemDocument)
strwrap(corpus[[1]])
dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm, 0.97)
dtm
labelledTerms = as.data.frame(as.matrix(dtm))
labelledTerms$Responsive = emails$responsive
str(labelledTerms)

library(caTools)
set.seed(144)
spl = sample.split(labelledTerms$Responsive, SplitRatio = 0.7)
train = subset(labelledTerms, spl == TRUE)
test = subset(labelledTerms, spl == FALSE)

library(rpart)
library(rpart.plot)
emailCART = rpart(Responsive ~ ., data = train, method = 'class')
prp(emailCART)

pred = predict(emailCART, newdata = test)
pred[1:10,]
pred.prob = pred[,2]
table(test$Responsive, pred.prob > 0.5)
accuracy = (195 + 25) / nrow(test)
accuracy
table(test$Responsive)
baselineAccuracy = 215 / nrow(test)
baselineAccuracy

library(ROCR)
predROCR = prediction(pred.prob, test$Responsive)
perf = performance(predROCR, 'tpr', 'fpr')
plot(perf, colorize = TRUE)
performance(predROCR, 'auc')@y.values
