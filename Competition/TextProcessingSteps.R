library(tm)
getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Competition")
getwd()
nyTrain = read.csv('NYTimesBlogTrain.csv', stringsAsFactors = FALSE)
nyTest = read.csv('NYTimesBlogTest.csv', stringsAsFactors = FALSE)
nyTest$Popular = NA
nyMerged = rbind(nyTrain, nyTest)

#Bag of words - Abstract
absCorpus = Corpus(VectorSource(nyMerged$Abstract))
absCorpus = tm_map(absCorpus, tolower)
absCorpus = tm_map(absCorpus, PlainTextDocument)
absCorpus = tm_map(absCorpus, removePunctuation)
absCorpus = tm_map(absCorpus, removeWords, stopwords('English'))
absCorpus = tm_map(absCorpus, stemDocument)

absDTM = DocumentTermMatrix(absCorpus)
absDTM

absSparse = removeSparseTerms(absDTM, 0.98)
absSparse

absSparse = as.data.frame(as.matrix(absSparse))
str(absSparse)

#Bag of words - Headlines
hlCorpus = Corpus(VectorSource(nyMerged$Headline))
hlCorpus[[1]]
hlCorpus = tm_map (hlCorpus, tolower)
hlCorpus = tm_map (hlCorpus, PlainTextDocument)
hlCorpus = tm_map (hlCorpus, removePunctuation)
hlCorpus = tm_map (hlCorpus, removeWords, stopwords('English'))
hlCorpus = tm_map (hlCorpus, stemDocument)

hlDTM = DocumentTermMatrix(hlCorpus)
hlDTM

hlSparse = removeSparseTerms(hlDTM, 0.98)
hlSparse

hlSparse = as.data.frame(as.matrix(hlSparse))

#Bag of words - Snippet
snippetCorpus = Corpus(VectorSource(nyMerged$Snippet))
snippetCorpus = tm_map (snippetCorpus, tolower)
snippetCorpus = tm_map (snippetCorpus, PlainTextDocument)
snippetCorpus = tm_map (snippetCorpus, removePunctuation)
snippetCorpus = tm_map (snippetCorpus, removeWords, stopwords('English'))
snippetCorpus = tm_map (snippetCorpus, stemDocument)

snippetDTM = DocumentTermMatrix(snippetCorpus)
snippetDTM

snippetSparse = removeSparseTerms(snippetDTM, 0.98)
snippetSparse

snippetSparse = as.data.frame(as.matrix(snippetSparse))

#Merge all data sets into one

nyAll = cbind(nyMerged, absSparse, hlSparse, snippetSparse)
colnames(nyAll) = make.names(colnames(nyAll))
str(nyAll)

nyAll$Abstract = NULL
nyAll$Headline = NULL
nyAll$Snippet = NULL
nyAll$PubDate = NULL

nyAll$NewsDesk = as.factor(nyAll$NewsDesk)
nyAll$SectionName = as.factor(nyAll$SectionName)
nyAll$SubsectionName = as.factor(nyAll$SubsectionName)
str(nyAll)

nyTrain = subset(nyAll, !is.na(Popular))
nyTest = subset(nyAll, is.na(Popular))
str(nyTrain)
str(nyTest)

