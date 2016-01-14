getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week7/Assignment/')
getwd()
library(ggplot2)
library(ggmap)
library(maps)

statesMap = map_data('state')

#1.1
str(statesMap)
length(unique(statesMap$group))

#1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(color = 'black', fill = 'white')

#2.1
polling = read.csv('PollingImputed.csv')
str(polling)
train = subset(polling, polling$Year %in% c(2004, 2008))
unique(train$Year)
test = subset(polling, polling$Year == 2012)
unique(test$Year)
logModel = glm(Republican ~ SurveyUSA + DiffCount, data = train, family = binomial)
testPredict = predict(logModel, newdata = test, type = 'response')
testPredictionBinary = as.numeric(testPredict > 0.5)

predictionDataFrame = data.frame(testPredict, testPredictionBinary, test$State)
str(predictionDataFrame)

table(predictionDataFrame$testPredictionBinary)
mean(predictionDataFrame$testPredict)

#2.2
predictionDataFrame$region = tolower(predictionDataFrame$test.State)
predictionMap = merge(predictionDataFrame, statesMap, by = 'region')
predictionMap = predictionMap[order(predictionMap$order), ]
str(predictionMap)
str(statesMap)

#2.4
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + 
        geom_polygon(color = 'black')

#2.5
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + 
        geom_polygon(color = 'black') + 
        scale_fill_gradient(high = 'red', low = 'blue', guide = 'legend', breaks = c(0,1), 
                            labels = c('Republican', 'Democrat'), name = 'Prediction 2012')

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredict)) + 
        geom_polygon(color = 'black') + 
        scale_fill_gradient(high = 'red', low = 'blue', guide = 'legend', breaks = c(0, 1),  
                            labels = c('Republican', 'Democrat'), name = 'Prediction 2012')

#3.2
head(subset(predictionMap, region == 'florida'))

#4.1
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredict)) + 
        geom_polygon(color = 'black', linetype = 3) + 
        scale_fill_gradient(high = 'red', low = 'blue', guide = 'legend', breaks = c(0, 1),  
                            labels = c('Republican', 'Democrat'), name = 'Prediction 2012')

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredict)) + 
        geom_polygon(color = 'black', linetype = 1, size = 3) + 
        scale_fill_gradient(high = 'red', low = 'blue', guide = 'legend', breaks = c(0, 1),  
                            labels = c('Republican', 'Democrat'), name = 'Prediction 2012')

#4.2
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredict)) + 
        geom_polygon(color = 'black', alpha = 0.3) + 
        scale_fill_gradient(high = 'red', low = 'blue', guide = 'legend', breaks = c(0, 1),  
                            labels = c('Republican', 'Democrat'), name = 'Prediction 2012')


