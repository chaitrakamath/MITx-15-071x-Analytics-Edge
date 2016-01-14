getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week6/Assignment/')
getwd()

#1.1
dailykos = read.csv('dailykos.csv')
str(dailykos)
distance = dist(dailykos, method = 'euclidean')
clusterArticles = hclust(distance, method = 'ward.D')

#1.2
plot(clusterArticles)

#1.4    
clusterGroups = cutree(clusterArticles, k = 7)
allGroups = sapply(1:7, function(x) colMeans(subset(dailykos, clusterGroups == x)))
str(allGroups)

#1.5
# hierCluster1 = subset(dailykos, clusterGroups == 1)
# hierCluster2 = subset(dailykos, clusterGroups == 2)
# hierCluster3 = subset(dailykos, clusterGroups == 3)
# hierCluster4 = subset(dailykos, clusterGroups == 4)
# hierCluster5 = subset(dailykos, clusterGroups == 5)
# hierCluster6 = subset(dailykos, clusterGroups == 6)
# hierCluster7 = subset(dailykos, clusterGroups == 7)
# 
# str(hierCluster1)
# str(hierCluster2)

tail(sort(allGroups[, 1]))

#1.6
#tail(apply(allGroups, 2, sort))
tail(sort(allGroups[, 1]))
tail(sort(allGroups[, 2]))
tail(sort(allGroups[, 3]))
tail(sort(allGroups[, 4]))
tail(sort(allGroups[, 5]))
tail(sort(allGroups[, 6]))
tail(sort(allGroups[, 7]))

#2.1
k = 7
set.seed(1000)
kmc = kmeans(dailykos, centers = k)
str(kmc)
kClustersCount = lapply(1:7, function(x) nrow(subset(dailykos, kmc$cluster == x)))
kClustersCount
kClusters = lapply(1:7, function(x) subset(dailykos, kmc$cluster == x))
str(kClusters)
tail(sort(colSums(kClusters[[1]])))
tail(sort(colSums(kClusters[[2]])))
tail(sort(colSums(kClusters[[3]])))
tail(sort(colSums(kClusters[[4]])))
tail(sort(colSums(kClusters[[5]])))
tail(sort(colSums(kClusters[[6]])))
tail(sort(colSums(kClusters[[7]])))
apply(1:7, function(x) tail(sort(colSums(kClusters[[x]]))))

table(clusterGroups, kmc$cluster)
