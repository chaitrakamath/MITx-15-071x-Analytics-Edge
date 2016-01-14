getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/FinalExam')
getwd()

#Q1
ebay = read.csv('ebay.csv', stringsAsFactors = FALSE)
str(ebay)
nrow(ebay[ebay$sold == 1, ]) / nrow(ebay)

#Q2
summary(ebay)

#Q3
table(ebay$size)

#Q4
ebay$sold = as.factor(ebay$sold)
ebay$condition = as.factor(ebay$condition)
ebay$heel = as.factor(ebay$heel)
ebay$style = as.factor(ebay$style)
ebay$color = as.factor(ebay$color)
ebay$material = as.factor(ebay$material)

#Q5
library(caTools)
set.seed(144)
spl = sample.split(ebay$sold, SplitRatio = 0.7)
ebayTrain = subset(ebay, spl == TRUE)
ebayTest = subset(ebay, spl == FALSE)

#Q6
logModel = glm(sold ~ biddable + startprice + condition + heel + style + color + material, 
               data = ebayTrain, family = 'binomial')
summary(logModel)

#Q7
biddable = as.integer(0)
startprice = 100
condition = as.factor(as.character('Pre-owned'))
heel = as.factor(as.character('High'))
style = as.factor(as.character('Open Toe'))
color = as.factor(as.character('Black'))
material = as.factor(as.character('Satin'))
mydata = data.frame(biddable, startprice, condition, heel, style, color, material)
predict(logModel, newdata = mydata, type = 'response')

#Q8
(exp(0.8325406) - 1) * 100

#Q9
predictedProb = predict(logModel, newdata = ebayTest, type = 'response')
table(ebayTest$sold, predictedProb > 0.5)

#Q10
library(ROCR)
predictionValues = prediction(predictedProb, ebayTest$sold)
perf.auc = performance(predictionValues, 'auc')
auc = as.numeric(perf.auc@y.values)
auc

#Q13
perf = performance(predictionValues, 'tpr', 'fpr')
plot(perf, colorize = TRUE)

#Q15
install.packages('caret')
library(caret)
library(e1071)

set.seed(144)
numFolds = trainControl(method = 'cv', number = 10)
cpGrid = expand.grid(.cp = seq(from = 0.001, to = 0.05, length.out = 50))

train(sold ~ biddable + startprice + condition + heel + style + color + material,
      data = ebayTrain, method = 'rpart', trControl = numFolds, tuneGrid = cpGrid)

#Q16
library(rpart)
library(rpart.plot)
ebayCART = rpart(sold ~ biddable + startprice + condition + heel + style + color + material,
                 data = ebayTrain, method = 'class', cp = 0.005)
prp(ebayCART)

#Q17
library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(ebay$description))
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus[[1]]
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]
corpus = tm_map(corpus, removeWords, stopwords('English'))
corpus[[1]]
corpus = tm_map(corpus, stemDocument)
corpus[[1]]

dtm = DocumentTermMatrix(corpus)
dtm

#Q18
spdtm = removeSparseTerms(dtm, 0.90)
spdtm

#Q19
descriptionText = as.data.frame(as.matrix(spdtm))
str(descriptionText)
sort(colSums(descriptionText))

#Q20
names(descriptionText) = paste0('D', names(descriptionText))
descriptionText$sold = ebay$sold
descriptionText$biddable = ebay$biddable
descriptionText$startprice = ebay$startprice
descriptionText$condition = ebay$condition
descriptionText$heel = ebay$heel
descriptionText$style = ebay$style
descriptionText$color = ebay$color
descriptionText$material = ebay$material

set.seed(144)
spl = sample.split(descriptionText$sold, SplitRatio = 0.7)
trainText = subset(descriptionText, spl == TRUE)
testText = subset(descriptionText, spl == FALSE)
str(testText)        

#Q21
glmText = glm(sold ~ ., data = trainText, family = binomial)
summary(glmText)

#Q22
trainPred = predict(glmText, type = 'response')
trainPredictions = prediction(trainPred, trainText$sold)
perf.aucTrain = performance(trainPredictions, 'auc')
trainAUC = as.numeric(perf.aucTrain@y.values)
trainAUC

textPred = predict(glmText, newdata = testText, type = 'response')
textPredictions = prediction(textPred, testText$sold)
perf.auc = performance(textPredictions, 'auc')
testAUC = as.numeric(perf.auc@y.values)
testAUC
