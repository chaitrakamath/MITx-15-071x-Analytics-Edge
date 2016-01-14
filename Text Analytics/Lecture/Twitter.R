setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week5/Lecture')
tweets = read.csv('tweets.csv', stringsAsFactors = FALSE)
str(tweets)
tweetsNegative = as.factor(tweets$Avg <= -1)
table(tweetsNegative)
install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus = tm_map (corpus, tolower)
corpus[[1]]
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map (corpus, removePunctuation)
corpus[[1]]
stopwords('English')[1:10]
corpus = tm_map (corpus, removeWords, c('apple', stopwords('English')))
corpus[[1]]
corpus = tm_map (corpus, stemDocument)
corpus[[1]]

frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)
sparse = removeSparseTerms(frequencies, 0.995)
sparse
tweets.sparse = as.data.frame(as.matrix(sparse))
colnames(tweets.sparse) = make.names(colnames(tweets.sparse))
tweets.sparse$Negative = tweetsNegative


library(caTools)
set.seed(123)
spl = sample.split(tweets.sparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweets.sparse, spl == TRUE)
testSparse = subset(tweets.sparse, spl == FALSE)

#Quiz
findFreqTerms(frequencies, lowfreq = 100)
#######

library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data = trainSparse, method = 'class')
prp(tweetCART)

predictCART = predict(tweetCART, newdata = testSparse, type = 'class')
table(testSparse$Negative, predictCART)
accuracy = (294 + 18) / nrow(testSparse)
accuracy

table(testSparse$Negative)
baselineAccuracy = 300 / nrow(testSparse)
baselineAccuracy

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data = trainSparse)
predictRF = predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predictRF)
rfAccuracy = (293 + 21) / nrow(testSparse)
rfAccuracy

#Quiz
tweetLog = glm(Negative ~ ., data = trainSparse, family = 'binomial')
predictions = predict(tweetLog, newdata = testSparse, type = 'response')
table(testSparse$Negative, predictions > 0.5)
logAccuracy = (253 + 32) / nrow(testSparse)
logAccuracy
####