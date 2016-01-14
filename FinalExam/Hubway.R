getwd()
setwd("/Users/admin/Documents/Coursera/AnalyticsEdge-edX/FinalExam")
getwd()

#Q1
hubwayData = read.csv('HubwayTrips.csv')
str(hubwayData)
nrow(hubwayData)

#Q2
mean(hubwayData$Duration)
mean(hubwayData[hubwayData$Weekday == 1, 'Duration'])
mean(hubwayData[hubwayData$Weekday == 0, 'Duration'])

#Q3
table(hubwayData$Morning)
table(hubwayData$Afternoon)
table(hubwayData$Evening)

#Q4
length(hubwayData$Male[hubwayData$Male == 1]) / nrow(hubwayData)

#Q5
str(hubwayData) #choose the variable with higher values

#Q6
library(caret)
preproc = preProcess(hubwayData)
hubwayNorm = predict(preproc, hubwayData)

max(hubwayNorm$Duration)
max(hubwayNorm$Age)

#Q8
k = 10
set.seed(5000)
hubwayKMC = kmeans(hubwayNorm, centers = k)
str(hubwayKMC)
sort(table(hubwayKMC$cluster))

#Q9
hubwaySubset = hubwayData[hubwayData$Male == 0 & hubwayData$Weekday == 1 & hubwayData$Evening == 1, ]
table(hubwayKMC$cluster[as.numeric(row.names(hubwaySubset))])

#Q10
meanDuration = mean(hubwayData$Duration)
hubwaySub = hubwayData[hubwayData$Duration > meanDuration & hubwayData$Afternoon == 1 & 
                               hubwayData$Weekday == 0, ]
head(hubwaySub)
head(as.numeric(row.names(hubwaySub)))
table(hubwayKMC$cluster[as.numeric(row.names(hubwaySub))])

#Q11
meanAge = mean(hubwayData$Age)
hubwaySub2 = hubwayData[hubwayData$Morning == 1 & hubwayData$Age > meanAge, ]
str(hubwaySub2)
table(hubwayKMC$cluster[as.numeric(row.names(hubwaySub2))])

#Q14
k = 20
set.seed(8000)
hubwayKMC2 = kmeans(hubwayNorm, centers = k)
str(hubwayKMC2)
sort(table(hubwayKMC2$cluster))

#Q15
hubwaySub3 = hubwayData[hubwayData$Duration < meanDuration & hubwayData$Evening == 1 & 
                                hubwayData$Weekday == 1, ]
head(as.numeric(row.names(hubwaySub3)))
table(hubwayKMC2$cluster[as.numeric(row.names(hubwaySub3))])

#Q17
plot(hubwayData$Age ~ hubwayKMC$cluster, col = hubwayKMC$cluster)
boxplot(hubwayData$Age ~ hubwayKMC$cluster)
boxplot(hubwayKMC$cluster ~ hubwayData$Age)

library(ggplot2)
hubData = hubwayData
hubData$cluster = hubwayKMC$cluster
str(hubData)
ggplot(hubData, aes(x = Age, y = cluster)) + geom_histogram()
ggplot(hubData, aes(x = Age, y = cluster)) + geom_point()
