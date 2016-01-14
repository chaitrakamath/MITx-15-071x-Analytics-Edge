setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week5/Assignment')
wiki = read.csv('wiki.csv', stringsAsFactors = FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)

#1.1
table(wiki$Vandal)

#1.2
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
strwrap(corpusAdded[[1]])
corpusAdded = tm_map (corpusAdded, removeWords, stopwords('English'))
corpusAdded = tm_map (corpusAdded, stemDocument)
strwrap(corpusAdded[[1]])
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

#1.3
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

#1.4
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded)
colnames(wordsAdded) = paste('A', colnames(wordsAdded))
colnames(wordsAdded)

corpusRemoved = Corpus(VectorSource(wiki$Removed))
strwrap(corpusRemoved[[5]])
corpusRemoved = tm_map (corpusRemoved, removeWords, stopwords('English'))
corpusRemoved = tm_map (corpusRemoved, stemDocument)
strwrap(corpusRemoved[[5]])
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste('R', colnames(wordsRemoved))
ncol(wordsRemoved)

#1.5
wikiWords = cbind(wordsAdded, wordsRemoved)
str(wikiWords)
wikiWords$Vandal = wiki$Vandal
head(wikiWords$Vandal)

library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, spl == TRUE)
wikiTest = subset(wikiWords, spl == FALSE)
table(wikiTest$Vandal)
baselineAccuracy = 618 / nrow(wikiTest)
baselineAccuracy

#1.6
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal ~ ., data = wikiTrain, method = 'class')

wikiPred = predict(wikiCART, newdata = wikiTest, type = 'class')
table(wikiTest$Vandal, wikiPred)
CARTAccuracy = (618 + 12) / nrow(wikiTest)
CARTAccuracy

#1.7
prp(wikiCART)

#2.1
wikiWords2 = wikiWords 
wikiWords2$HTTP = ifelse(grepl ('http', wiki$Added, fixed = TRUE), 1, 0)
table(wikiWords2$HTTP)

#2.2
wikiTrain2 = subset(wikiWords2, spl == TRUE)
wikiTest2 = subset(wikiWords2, spl == FALSE)

wikiCART2 = rpart(Vandal ~ ., data = wikiTrain2)
wikiPred2 = predict(wikiCART2, newdata = wikiTest2, type = 'class')
table(wikiTest2$Vandal, wikiPred2)
accuracy = (609 + 57) / nrow(wikiTest2)
accuracy

#2.3
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

#2.4
wikiTrain3 = subset(wikiWords2, spl == TRUE)
wikiTest3 = subset(wikiWords2, spl == FALSE)

wikiCART3 = rpart(Vandal ~ ., data = wikiTrain3, method = 'class')
wikiPred3 = predict(wikiCART3, newdata = wikiTest3, type = 'class')
table(wikiTest3$Vandal, wikiPred3)
accuracy3 = (514 + 248) / nrow(wikiTest3)
accuracy3

#3.1
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$LoggedIn = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl == TRUE)
wikiTest4 = subset(wikiWords3, spl == FALSE)
wikiCART4 = rpart(Vandal ~ ., data = wikiTrain4, method = 'class')
wikiPred4 = predict(wikiCART4, newdata = wikiTest4, type = 'class')
table(wikiTest4$Vandal, wikiPred4)
accuracy4 = (595 + 241) / nrow(wikiTest4)
accuracy4

#3.2
prp(wikiCART4)
