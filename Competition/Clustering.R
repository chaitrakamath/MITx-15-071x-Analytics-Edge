library(tm)
getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Competition")
getwd()
nyTrain = read.csv('NYTimesBlogTrain.csv', stringsAsFactors = FALSE)
nyTest = read.csv('NYTimesBlogTest.csv', stringsAsFactors = FALSE)
nyTest$Popular = NA
nyMerged = rbind(nyTrain, nyTest)

#Bag of words - Headlines

grep('Obama | Apple | Twitter | First Draft | New York | ', nyMerged$Headline, value = TRUE)
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