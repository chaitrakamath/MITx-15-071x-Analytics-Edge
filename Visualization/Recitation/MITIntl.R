getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week7/Recitation/')
getwd()

library(ggplot2)
intl = read.csv('intl.csv')
str(intl)

#Bar plot
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = 'identity') + geom_text(aes(label = PercentOfIntl))

intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)
intl$PercentOfIntl = intl$PercentOfIntl * 100

ggplot(intl, aes(x = Region, y = PercentOfIntl)) + 
        geom_bar(stat = 'identity', fill = 'dark blue') + 
        geom_text(aes(label = PercentOfIntl), vjust = -0.4) +
        ylab ('Percent of International Students') +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
        

#World Map
library(ggmap)
intlall = read.csv('intlall.csv', stringsAsFactors = FALSE)
head(intlall)
intlall[is.na(intlall)] = 0
head(intlall)

worldMap = map_data('world')
str(worldMap)
worldMap = merge(worldMap, intlall, by.x = 'region', by.y = 'Citizenship')
str(worldMap)

ggplot(worldMap, aes(x = long, y = lat, group = group))+
        geom_polygon(fill = 'white', color = 'black') + 
        coord_map('mercator')

worldMap = worldMap[order(worldMap$group, worldMap$order), ]
ggplot(worldMap, aes(x = long, y = lat, group = group))+
        geom_polygon(fill = 'white', color = 'black') + 
        coord_map('mercator')

table(intlall$Citizenship)
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = 'China'
table(intlall$Citizenship)
worldMap = map_data('world')
str(worldMap)
worldMap = merge(worldMap, intlall, by.x = 'region', by.y = 'Citizenship')
worldMap = worldMap[order(worldMap$group, worldMap$order), ]

ggplot(worldMap, aes(x = long, y = lat, group = group))+
        geom_polygon(aes(fill = Total), color = 'black')+
        coord_map('mercator')

ggplot(worldMap, aes(x = long, y = lat, group = group))+
        geom_polygon(aes(fill = Total), color = 'black')+
        coord_map('ortho', orientation = c(20, 30, 0))

ggplot(worldMap, aes(x = long, y = lat, group = group))+
        geom_polygon(aes(fill = Total), color = 'black')+
        coord_map('ortho', orientation = c(-37, 175, 0))

#Line chart

install.packages('reshape2')
library(reshape2)
library(ggplot2)

household = read.csv('households.csv')
str(household)
household[, 1:2]
head(melt(household, id = 'Year'))
household[, 1:3]
melt(household, id = 'Year')[1:10,]

ggplot(melt(household, id = 'Year'), aes(x = Year, y = value, color = variable))+
        geom_line(size = 2) + geom_point(size = 5) + ylab('Percentage of households')
