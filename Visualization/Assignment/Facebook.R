getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week7/Assignment/')
getwd()
library(ggplot2)

#1.1
edges = read.csv('edges.csv')
str(edges)
users = read.csv('users.csv')
str(users)
length(users$id)
avgFriends = (2 * nrow(edges)) / nrow(users)
avgFriends

#1.2
str(users)
table (users$school, users$locale)

#1.3
table(users$school, users$gender)

#2.1
install.packages('igraph')
library(igraph)
?graph.data.frame

#2.2
g <- graph.data.frame(edges, directed = FALSE, vertices = users)
plot(g, vertex.size = 0.5, vertex.label = NA)

#2.3
sort(degree(g))
length(degree(g)[degree(g) >= 10])

#2.4
V(g)$size = degree(g) / 2 + 2
plot(g, vertex.label = NA)
sort(V(g)$size)

#3.1
V(g)$color = 'black'
V(g)$color[V(g)$gender == 'A'] = 'red'
V(g)$color[V(g)$gender == 'B'] = 'gray'
plot(g, vertex.label = NA)

table(V(g)$gender, degree(g))

#3.2
unique(V(g)$school)
V(g)$color = 'black'
V(g)$color[V(g)$school == 'A'] = 'red'
V(g)$color[V(g)$school == 'AB'] = 'gray'
plot(g, vertex.label = NA)

#3.3
unique(V(g)$locale)
V(g)$color = 'black'
V(g)$color[V(g)$locale == 'A'] = 'red'
V(g)$color[V(g)$locale == 'B'] = 'gray'
plot(g, vertex.label = NA)
