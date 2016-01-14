library(tm)
getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Competition")
getwd()
nyTrain = read.csv('NYTimesBlogTrain.csv', stringsAsFactors = FALSE)
nyTest = read.csv('NYTimesBlogTest.csv', stringsAsFactors = FALSE)
nyTest$Popular = NA
nyMerged = rbind(nyTrain, nyTest)

unique(nyMerged$SectionName)
nyMerged$SectionName[nyMerged$SectionName == ""] = "Other"
unique(nyMerged$SectionName)

crosswordsData = subset(nyMerged, SectionName == "Crosswords/Games")
artsData = subset(nyMerged, SectionName == "Arts")
businessData = subset(nyMerged, SectionName == "Business Day")
healthData = subset(nyMerged, SectionName == "Health")
opinionData = subset(nyMerged, SectionName == "Opinion")
otherData = subset(nyMerged, SectionName == "Other")
styleData = subset(nyMerged, SectionName == "Style")
worldData = subset(nyMerged, SectionName == "World")
techData = subset(nyMerged, SectionName == "Technology")
magazineData = subset(nyMerged, SectionName == "Magazine")
multimediaData = subset(nyMerged, SectionName == "Multimedia")
travelData = subset(nyMerged, SectionName == "Travel")
usData = subset(nyMerged, SectionName == "U.S.")
nyData = subset(nyMerged, SectionName == "N.Y. / Region")
openData = subset(nyMerged, SectionName == "Open")
sportsData = subset(nyMerged, SectionName == "Sports")

#Feature Engg
crosswordPopular = subset(crosswordsData, crosswordsData$Popular == 1)
crosswordPopularHeadlines = Corpus(VectorSource(crosswordPopular$Headline))
artsPopular = subset(artsData, artsData$Popular == 1)
nyPopular = subset(nyData, nyData$Popular == 1)
techPopular = subset(techData, techData$Popular == 1)
library(tm)
#Crossword data corpus
crosswordPopularHeadline = Corpus(VectorSource(crosswordsData$Headline)) 
crosswordPopularHeadline[[1]]
crosswordPopularHeadline = tm_map(crosswordPopularHeadline, tolower)
crosswordPopularHeadline[[1]]
crosswordPopularHeadline = tm_map(crosswordPopularHeadline, PlainTextDocument)
crosswordPopularHeadline[[1]]
crosswordPopularHeadline = tm_map(crosswordPopularHeadline, removeWords, stopwords('English'))
crosswordPopularHeadline[[1]]
crosswordDTM = DocumentTermMatrix(crosswordCorpus)
crosswordPopularSparse = removeSparseTerms(crosswordDTM, 0.99)
crosswordPopularSparse
popularCrosswordTerms = names(as.data.frame(as.matrix(crosswordPopularSparse)))


crosswordHeadline = as.data.frame(as.matrix(crosswordDTM))
crosswordSparse = removeSparseTerms(crosswordDTM, 0.98)
crosswordSparse
crosswordSparse = as.data.frame(as.matrix(crosswordSparse))
crosswordAll = cbind(crosswordsData, crosswordSparse)
table(crosswordAll$Popular, crosswordAll$Section)
subset(crosswordAll, crosswordAll$Popular == 1)
crosswordAll$Popular = as.factor(crosswordAll$Popular)
crosswordAll$NewsDesk = as.factor(crosswordAll$NewsDesk)
crosswordAll$SubsectionName = as.factor(crosswordAll$SubsectionName)
crosswordAll$acrostic = as.factor(crosswordAll$acrostic)
crosswordAll$crossword = as.factor(crosswordAll$crossword)
crosswordTrain = subset(crosswordAll, select = c('NewsDesk', 'SubsectionName', 'WordCount', 'acrostic'
                                                 , 'crossword', 'like', 'not', 'the', 'variety:', 'Popular'))
crosswordLog = glm(Popular ~ WordCount + acrostic + crossword, data = crosswordAll, 
                   family = binomial)
summary(crosswordLog)
