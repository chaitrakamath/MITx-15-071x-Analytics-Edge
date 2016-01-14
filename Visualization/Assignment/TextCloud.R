getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week7/Assignment/')
getwd()
library(ggplot2)

tweets = read.csv('tweets.csv', stringsAsFactors = FALSE)
str(tweets)

#1.1
library(tm)
tweetCorpus = Corpus(VectorSource(tweets$Tweet))
tweetCorpus = tm_map(tweetCorpus, tolower)
tweetCorpus = tm_map(tweetCorpus,PlainTextDocument)
tweetCorpus = tm_map(tweetCorpus, removePunctuation)
tweetCorpus = tm_map(tweetCorpus, removeWords, stopwords('English'))

tweetDTM = DocumentTermMatrix(tweetCorpus)
str(tweetDTM)
allTweets = as.data.frame(as.matrix(tweetDTM))
str(allTweets)

#2.1
install.packages('wordcloud')
library(wordcloud)
head(colnames(allTweets), 80)

#2.2
head(colSums(allTweets), 80)

#2.3
?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25))

#2.4
tweetCorpus = tm_map(tweetCorpus, removeWords, 'apple')
tweetDTM = DocumentTermMatrix(tweetCorpus)
allTweets = as.data.frame(as.matrix(tweetDTM))
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25))

#3
?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25), 
          colors = c('black','blue','red', 'orange', 'green'), random.color = TRUE)

#3.1
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

#3.2
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25), random.order = FALSE)

#3.3
wordcloud(colnames(negativeTweets), colSums(negativeTweets), rot.per = 0.1)

#3.5
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25), 
          colors = c('black','blue','red', 'orange', 'green'), random.color = FALSE)

#4.1
install.packages('RColorBrewer')
library(RColorBrewer)
display.brewer.all()

#4.3
?brewer.pal()
wordcloud(colnames(allTweets), colSums(allTweets), colors = brewer.pal(9, 'Blues'), scale = c(2, 0.25))
wordcloud(colnames(allTweets), colSums(allTweets), 
          colors = brewer.pal(9, 'Blues')[c(5, 6, 7, 8, 9)], scale = c(2, 0.25))
