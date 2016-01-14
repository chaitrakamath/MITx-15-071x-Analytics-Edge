getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week6/Lecture/')
movies = read.table('movielens.txt', header = FALSE, sep = '|', quote = "\"")
str(movies)
colnames(movies) = c('ID', 'Title','ReleaseDate', 'VideoReleaseDate', 'IMDB', 'Unknown', 'Action', 'Adventure', 'Animation', 
                     'Childrens', 'Comedy', 'Crime', 'Documentary', 'Drama', 'Fantasy', 'FilmNoir', 'Horror', 'Musical', 'Mystery', 
                     'Romance', 'SciFi', 'Thriller', 'War', 'Western')
str(movies)
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
str(movies)

#Quiz
table(movies$Comedy)
table(movies$Western)
table(movies$Romance)
table(movies$Drama)
table(movies$Romance & movies$Drama)
####

distances = dist(movies[2:20], method = 'euclidean')
clusterMovies = hclust (distances, method = 'ward.D')
plot(clusterMovies)
clusterGroups = cutree(clusterMovies, k = 10)
clusterGroups
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)
subset(movies, Title == 'Men in Black (1997)')
clusterGroups[257]
cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title

#Quiz
clusterGroup = cutree(clusterMovies, k = 2)
library(caTools)
spl = split(movies[2:20], clusterGroup)
lapply(spl, colMeans)
###