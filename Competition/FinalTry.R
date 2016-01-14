library(tm)
getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Competition")
getwd()
nyTrain = read.csv('NYTimesBlogTrain.csv', stringsAsFactors = FALSE)
nyTest = read.csv('NYTimesBlogTest.csv', stringsAsFactors = FALSE)
nyTest$Popular = NA
nyMerged = rbind(nyTrain, nyTest)
unique(nyMerged$NewsDesk)
nyMerged$NewsDesk[nyMerged$NewsDesk == ""] = 'Other'
unique(nyMerged$NewsDesk)

library(tm)

#Business Popular Terms
bizPopular = subset(nyMerged, nyMerged$NewsDesk == 'Business')
str(bizPopular)
bizPopularHeadlines = Corpus(VectorSource(bizPopular$Headline))
bizPopularHeadlines[[1]]
bizPopularHeadlines = tm_map(bizPopularHeadlines, tolower)
bizPopularHeadlines[[1]]
bizPopularHeadlines = tm_map(bizPopularHeadlines, PlainTextDocument)
bizPopularHeadlines[[1]]
bizPopularHeadlines = tm_map(bizPopularHeadlines, removeWords, stopwords('en'))
bizPopularHeadlines[[1]]
bizPopularHeadlines = tm_map (bizPopularHeadlines, removePunctuation)
bizPopularHeadlines[[1]]
bizPopularHeadlines = tm_map (bizPopularHeadlines, stemDocument)
bizPopularHeadlines[[1]]
bizPopDTM = DocumentTermMatrix(bizPopularHeadlines)
bizPopDTM
bizPopSparse = removeSparseTerms(bizPopDTM, 0.98)
bizPopSparse
popularBizTerms = names(as.data.frame(as.matrix(bizPopSparse)))
popularBizTerms

#Culture Popular Terms
culturePopular = subset(nyMerged, nyMerged$NewsDesk == 'Culture')
str(culturePopular)
culturePopularHeadlines = Corpus(VectorSource(culturePopular$Headline))
culturePopularHeadlines[[1]]
culturePopularHeadlines = tm_map(culturePopularHeadlines, tolower)
culturePopularHeadlines[[1]]
culturePopularHeadlines = tm_map(culturePopularHeadlines, PlainTextDocument)
culturePopularHeadlines[[1]]
culturePopularHeadlines = tm_map(culturePopularHeadlines, removeWords, stopwords('en'))
culturePopularHeadlines[[1]]
culturePopularHeadlines = tm_map (culturePopularHeadlines, removePunctuation)
culturePopularHeadlines[[1]]
culturePopularHeadlines = tm_map (culturePopularHeadlines, stemDocument)
culturePopularHeadlines[[1]]
culturePopDTM = DocumentTermMatrix(culturePopularHeadlines)
culturePopDTM
culturePopSparse = removeSparseTerms(culturePopDTM, 0.98)
culturePopSparse
popularcultureTerms = names(as.data.frame(as.matrix(culturePopSparse)))
popularcultureTerms

#Science Popular Terms
sciencePopular = subset(nyMerged, nyMerged$NewsDesk == 'Science')
str(sciencePopular)
sciencePopularHeadlines = Corpus(VectorSource(sciencePopular$Headline))
sciencePopularHeadlines[[1]]
sciencePopularHeadlines = tm_map(sciencePopularHeadlines, tolower)
sciencePopularHeadlines[[1]]
sciencePopularHeadlines = tm_map(sciencePopularHeadlines, PlainTextDocument)
sciencePopularHeadlines[[1]]
sciencePopularHeadlines = tm_map(sciencePopularHeadlines, removeWords, stopwords('en'))
sciencePopularHeadlines[[1]]
sciencePopularHeadlines = tm_map (sciencePopularHeadlines, removePunctuation)
sciencePopularHeadlines[[1]]
sciencePopularHeadlines = tm_map (sciencePopularHeadlines, stemDocument)
sciencePopularHeadlines[[1]]
sciencePopDTM = DocumentTermMatrix(sciencePopularHeadlines)
sciencePopDTM
sciencePopSparse = removeSparseTerms(sciencePopDTM, 0.98)
sciencePopSparse
popularscienceTerms = names(as.data.frame(as.matrix(sciencePopSparse)))
popularscienceTerms

#OpEd Popular Terms
opedPopular = subset(nyMerged, nyMerged$NewsDesk == 'OpEd')
str(opedPopular)
opedPopularHeadlines = Corpus(VectorSource(opedPopular$Headline))
opedPopularHeadlines[[1]]
opedPopularHeadlines = tm_map(opedPopularHeadlines, tolower)
opedPopularHeadlines[[1]]
opedPopularHeadlines = tm_map(opedPopularHeadlines, PlainTextDocument)
opedPopularHeadlines[[1]]
opedPopularHeadlines = tm_map(opedPopularHeadlines, removeWords, stopwords('en'))
opedPopularHeadlines[[1]]
opedPopularHeadlines = tm_map (opedPopularHeadlines, removePunctuation)
opedPopularHeadlines[[1]]
opedPopularHeadlines = tm_map (opedPopularHeadlines, stemDocument)
opedPopularHeadlines[[1]]
opedPopDTM = DocumentTermMatrix(opedPopularHeadlines)
opedPopDTM
opedPopSparse = removeSparseTerms(opedPopDTM, 0.99)
opedPopSparse
popularopedTerms = names(as.data.frame(as.matrix(opedPopSparse)))
popularopedTerms

#Other Popular Terms
otherPopular = subset(nyMerged, nyMerged$NewsDesk == 'Other')
str(otherPopular)
otherPopularHeadlines = Corpus(VectorSource(otherPopular$Headline))
otherPopularHeadlines[[1]]
otherPopularHeadlines = tm_map(otherPopularHeadlines, tolower)
otherPopularHeadlines[[1]]
otherPopularHeadlines = tm_map(otherPopularHeadlines, PlainTextDocument)
otherPopularHeadlines[[1]]
otherPopularHeadlines = tm_map(otherPopularHeadlines, removeWords, stopwords('en'))
otherPopularHeadlines[[1]]
otherPopularHeadlines = tm_map (otherPopularHeadlines, removePunctuation)
otherPopularHeadlines[[1]]
otherPopularHeadlines = tm_map (otherPopularHeadlines, stemDocument)
otherPopularHeadlines[[1]]
otherPopDTM = DocumentTermMatrix(otherPopularHeadlines)
otherPopDTM
otherPopSparse = removeSparseTerms(otherPopDTM, 0.98)
otherPopSparse
popularotherTerms = names(as.data.frame(as.matrix(otherPopSparse)))
popularotherTerms

#Foreign Popular Terms
foreignPopular = subset(nyMerged, nyMerged$NewsDesk == 'Foreign')
str(foreignPopular)
foreignPopularHeadlines = Corpus(VectorSource(foreignPopular$Headline))
foreignPopularHeadlines[[1]]
foreignPopularHeadlines = tm_map(foreignPopularHeadlines, tolower)
foreignPopularHeadlines[[1]]
foreignPopularHeadlines = tm_map(foreignPopularHeadlines, PlainTextDocument)
foreignPopularHeadlines[[1]]
foreignPopularHeadlines = tm_map(foreignPopularHeadlines, removeWords, stopwords('en'))
foreignPopularHeadlines[[1]]
foreignPopularHeadlines = tm_map (foreignPopularHeadlines, removePunctuation)
foreignPopularHeadlines[[1]]
foreignPopularHeadlines = tm_map (foreignPopularHeadlines, stemDocument)
foreignPopularHeadlines[[1]]
foreignPopDTM = DocumentTermMatrix(foreignPopularHeadlines)
foreignPopDTM
foreignPopSparse = removeSparseTerms(foreignPopDTM, 0.98)
foreignPopSparse
popularforeignTerms = names(as.data.frame(as.matrix(foreignPopSparse)))
popularforeignTerms

#Styles Popular Terms
stylePopular = subset(nyMerged, nyMerged$NewsDesk == 'Styles')
str(stylePopular)
stylePopularHeadlines = Corpus(VectorSource(stylePopular$Headline))
stylePopularHeadlines[[1]]
stylePopularHeadlines = tm_map(stylePopularHeadlines, tolower)
stylePopularHeadlines[[1]]
stylePopularHeadlines = tm_map(stylePopularHeadlines, PlainTextDocument)
stylePopularHeadlines[[1]]
stylePopularHeadlines = tm_map(stylePopularHeadlines, removeWords, stopwords('en'))
stylePopularHeadlines[[1]]
stylePopularHeadlines = tm_map (stylePopularHeadlines, removePunctuation)
stylePopularHeadlines[[1]]
stylePopularHeadlines = tm_map (stylePopularHeadlines, stemDocument)
stylePopularHeadlines[[1]]
stylePopDTM = DocumentTermMatrix(stylePopularHeadlines)
stylePopDTM
stylePopSparse = removeSparseTerms(stylePopDTM, 0.98)
stylePopSparse
popularstyleTerms = names(as.data.frame(as.matrix(stylePopSparse)))
popularstyleTerms

#TStyle Popular Terms
tStylePopular = subset(nyMerged, nyMerged$NewsDesk == 'TStyle')
str(tStylePopular)
tStylePopularHeadlines = Corpus(VectorSource(tStylePopular$Headline))
tStylePopularHeadlines[[1]]
tStylePopularHeadlines = tm_map(tStylePopularHeadlines, tolower)
tStylePopularHeadlines[[1]]
tStylePopularHeadlines = tm_map(tStylePopularHeadlines, PlainTextDocument)
tStylePopularHeadlines[[1]]
tStylePopularHeadlines = tm_map(tStylePopularHeadlines, removeWords, stopwords('en'))
tStylePopularHeadlines[[1]]
tStylePopularHeadlines = tm_map (tStylePopularHeadlines, removePunctuation)
tStylePopularHeadlines[[1]]
tStylePopularHeadlines = tm_map (tStylePopularHeadlines, stemDocument)
tStylePopularHeadlines[[1]]
tStylePopDTM = DocumentTermMatrix(tStylePopularHeadlines)
tStylePopDTM
tStylePopSparse = removeSparseTerms(tStylePopDTM, 0.98)
tStylePopSparse
populartStyleTerms = names(as.data.frame(as.matrix(tStylePopSparse)))
populartStyleTerms

#Magazine Popular Terms -- no popular terms in Magazine
magazinePopular = subset(nyMerged, nyMerged$NewsDesk == 'Magazine')
str(magazinePopular)
magazinePopularHeadlines = Corpus(VectorSource(magazinePopular$Headline))
magazinePopularHeadlines[[1]]
magazinePopularHeadlines = tm_map(magazinePopularHeadlines, tolower)
magazinePopularHeadlines[[1]]
magazinePopularHeadlines = tm_map(magazinePopularHeadlines, PlainTextDocument)
magazinePopularHeadlines[[1]]
magazinePopularHeadlines = tm_map(magazinePopularHeadlines, removeWords, stopwords('en'))
magazinePopularHeadlines[[1]]
magazinePopularHeadlines = tm_map (magazinePopularHeadlines, removePunctuation)
magazinePopularHeadlines[[1]]
magazinePopularHeadlines = tm_map (magazinePopularHeadlines, stemDocument)
magazinePopularHeadlines[[1]]
magazinePopDTM = DocumentTermMatrix(magazinePopularHeadlines)
magazinePopDTM
magazinePopSparse = removeSparseTerms(magazinePopDTM, 0.95)
magazinePopSparse
popularmagazineTerms = names(as.data.frame(as.matrix(magazinePopSparse)))
popularmagazineTerms


#Travel Popular Terms
travelPopular = subset(nyMerged, nyMerged$NewsDesk == 'Travel')
str(travelPopular)
travelPopularHeadlines = Corpus(VectorSource(travelPopular$Headline))
travelPopularHeadlines[[1]]
travelPopularHeadlines = tm_map(travelPopularHeadlines, tolower)
travelPopularHeadlines[[1]]
travelPopularHeadlines = tm_map(travelPopularHeadlines, PlainTextDocument)
travelPopularHeadlines[[1]]
travelPopularHeadlines = tm_map(travelPopularHeadlines, removeWords, stopwords('en'))
travelPopularHeadlines[[1]]
travelPopularHeadlines = tm_map (travelPopularHeadlines, removePunctuation)
travelPopularHeadlines[[1]]
travelPopularHeadlines = tm_map (travelPopularHeadlines, stemDocument)
travelPopularHeadlines[[1]]
travelPopDTM = DocumentTermMatrix(travelPopularHeadlines)
travelPopDTM
travelPopSparse = removeSparseTerms(travelPopDTM, 0.99)
travelPopSparse
populartravelTerms = names(as.data.frame(as.matrix(travelPopSparse)))
populartravelTerms

#Metro Popular Terms
metroPopular = subset(nyMerged, nyMerged$NewsDesk == 'Metro')
str(metroPopular)
metroPopularHeadlines = Corpus(VectorSource(metroPopular$Headline))
metroPopularHeadlines[[1]]
metroPopularHeadlines = tm_map(metroPopularHeadlines, tolower)
metroPopularHeadlines[[1]]
metroPopularHeadlines = tm_map(metroPopularHeadlines, PlainTextDocument)
metroPopularHeadlines[[1]]
metroPopularHeadlines = tm_map(metroPopularHeadlines, removeWords, stopwords('en'))
metroPopularHeadlines[[1]]
metroPopularHeadlines = tm_map (metroPopularHeadlines, removePunctuation)
metroPopularHeadlines[[1]]
metroPopularHeadlines = tm_map (metroPopularHeadlines, stemDocument)
metroPopularHeadlines[[1]]
metroPopDTM = DocumentTermMatrix(metroPopularHeadlines)
metroPopDTM
metroPopSparse = removeSparseTerms(metroPopDTM, 0.98)
metroPopSparse
popularmetroTerms = names(as.data.frame(as.matrix(metroPopSparse)))
popularmetroTerms

#National Popular Terms -- no popular terms
nationalPopular = subset(nyMerged, nyMerged$NewsDesk == 'National')
str(nationalPopular)


#Sports Popular Terms -- no popular terms
sportsPopular = subset(nyMerged, nyMerged$NewsDesk == 'Sports')
str(sportsPopular)

nyMergedHeadlines = Corpus(VectorSource(nyMerged$Headline))
nyMergedHeadlines[[1]]
nyMergedHeadlines = tm_map(nyMergedHeadlines, tolower)
nyMergedHeadlines[[1]]
nyMergedHeadlines = tm_map(nyMergedHeadlines, PlainTextDocument)
nyMergedHeadlines[[1]]
nyMergedHeadlines = tm_map(nyMergedHeadlines, removeWords, stopwords('en'))
nyMergedHeadlines[[1]]
nyMergedHeadlines = tm_map (nyMergedHeadlines, removePunctuation)
nyMergedHeadlines[[1]]
nyMergedHeadlines = tm_map (nyMergedHeadlines, stemDocument)
nyMergedHeadlines[[1]]
nyMergedDTM = DocumentTermMatrix(nyMergedHeadlines)
nyMergedDTM
nyMergedSparse = removeSparseTerms(nyMergedDTM, 0.99)
nyMergedSparse
nyMergedFreqTerms = as.data.frame(as.matrix(nyMergedSparse))
colnames(nyMergedFreqTerms) = make.names(colnames(nyMergedFreqTerms))
str(nyMergedFreqTerms)

nyAll = cbind(nyMerged, nyMergedFreqTerms)

allData = nyAll
allData$Headline = NULL
allData$Snippet = NULL
allData$Abstract = NULL
allData$NewsDesk = as.factor(allData$NewsDesk)
allData$SectionName = as.factor(allData$SectionName)
allData$SubsectionName = as.factor(allData$SubsectionName)
allData$Popular = as.factor(allData$Popular)
str(allData)

trainData = subset(allData, is.na(allData$Popular) == FALSE)
str(trainData)
testData = subset(allData, is.na(allData$Popular) == TRUE)
str(testData)

trainData$PubDate = NULL
str(trainData)
testData$PubDate = NULL
testData$Popular = NULL
str(testData)

#Logistic Regression
simpleLogModel = glm(Popular ~ NewsDesk + SectionName + SubsectionName  + WordCount, data = trainData, 
                     family = binomial)
summary(simpleLogModel)
simpleLogPredict = predict(simpleLogModel, type = 'response')
table(trainData$Popular, simpleLogPredict > 0.5)
(5201 + 718) / nrow(trainData)


logModel = glm(Popular ~ ., data = trainData, family = binomial)
summary(logModel)

str(testData)
logPredict = predict(logModel, type = 'response', newdata = testData)
logPredictTrain = predict(logModel, type = 'response')
table(trainData$Popular, logPredictTrain > 0.5)
(5215 + 719) / nrow(trainData)

mySubmission = data.frame(UniqueID = testData$UniqueID, Probability1 = logPredict)
write.csv(mySubmission, "logPredictions.csv", row.names=FALSE)
#(5188 + 750) / nrow(trainData)
#******************************************************************************************

#Random Forest
library(randomForest)
trainRF = trainData
trainRF$Popular = as.numeric(as.character(trainRF$Popular))
str(trainRF)
set.seed(1)
selectedMTry = tuneRF(x = trainRF[ , - trainRF$Popular ], y = trainRF[ , 'Popular'])

set.seed(1)
simpleRFModel = randomForest(Popular ~ ., data = nyTrain, ntree = 1000, nodesize = 5, mytry = 45)
simpleRFPredict = predict(rfModel, type = 'response')
table(nyTrain$Popular, simpleRFPredict > 0.5)
(5245 + 676) / nrow(nyTrain)

simpleEnsemblePred = (simpleRFPredict + simpleLogPredict) / 2
table(nyTrain$Popular, simpleEnsemblePred > 0.5)
(5223 + 700) / nrow(nyTrain)

set.seed(1)
rfModel = randomForest(Popular ~ ., data = trainRF, ntree = 1000, nodesize = 5, mytry = 15)
rfPredict = predict(rfModel, type = 'response', newdata = testData)
rfPredictTrain = predict(rfModel, type = 'response')
table(trainRF$Popular, rfPredictTrain > 0.5)
(5216 + 742) /  nrow(nyTrain)

ensemblePred = (logPredict + rfPredict) / 2

mySubmission = data.frame(UniqueID = testData$UniqueID, Probability1 = rfPredict)
write.csv(mySubmission, "tunedRFv2.csv", row.names=FALSE)


table(trainRF$Popular, ensemblePred > 0.5)
(5208 + 746) / nrow(nyTrain)
#******************************************************************************************

