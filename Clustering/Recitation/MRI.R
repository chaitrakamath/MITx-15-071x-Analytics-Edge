getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week6/Recitation/')
getwd()

healthy = read.csv('healthy.csv', header = FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes = FALSE, col = grey(seq(0, 1, length = 256)))

healthyVector = as.vector(healthyMatrix)
str(healthyVector)
n = length(healthyVector)
n * (n - 1) / 2 #This number is large. Hence, computing distance would throw memory error
#distance = distance(healthyVector, method = 'euclidean') #skip running this step to avoid R hanging

k = 5
set.seed(1)
kmc = kmeans(healthyVector,centers = k, iter.max = 1000)
str(kmc)
healthyClusters = kmc$cluster
kmc$centers[2]

dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col = rainbow(k))

tumor = read.csv('tumor.csv', header = FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

install.packages('flexclust')
library(flexclust)

kmc.kcca = as.kcca (kmc, healthyVector)
tumorClusters = predict(kmc.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col = rainbow(k))
