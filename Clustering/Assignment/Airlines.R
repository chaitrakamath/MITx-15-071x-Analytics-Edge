getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week6/Assignment/')
getwd()

#1.1
airlines = read.csv('AirlinesCluster.csv')
str(airlines)
summary(airlines)

#1.3
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

#2.1
distance = dist(airlinesNorm, method = 'euclidean')
hierCluster = hclust(distance, method = 'ward.D')
plot(hierCluster)

#2.2
clusterGroups = cutree(hierCluster, k = 5)
table(clusterGroups)

sort(tapply(airlines$Balance, clusterGroups, mean))
sort(tapply(airlines$QualMiles, clusterGroups, mean))
sort(tapply(airlines$BonusMiles, clusterGroups, mean))
sort(tapply(airlines$BonusTrans, clusterGroups, mean))
sort(tapply(airlines$FlightMiles, clusterGroups, mean))
sort(tapply(airlines$FlightTrans, clusterGroups, mean))
sort(tapply(airlines$DaysSinceEnroll, clusterGroups, mean))


#3.1
k = 5
set.seed(88)
kmc = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(kmc)
table(kmc$cluster)

#3.2
kmc$centers
sort(tapply(airlines$Balance, kmc$cluster, mean))
sort(tapply(airlines$QualMiles, kmc$cluster, mean))
sort(tapply(airlines$BonusMiles, kmc$cluster, mean))
sort(tapply(airlines$BonusTrans, kmc$cluster, mean))
sort(tapply(airlines$FlightMiles, kmc$cluster, mean))
sort(tapply(airlines$FlightTrans, kmc$cluster, mean))
sort(tapply(airlines$DaysSinceEnroll, kmc$cluster, mean))

