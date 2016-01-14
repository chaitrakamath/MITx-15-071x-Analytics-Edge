getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week7/Lecture/')
getwd()

mvt = read.csv('mvt.csv', stringsAsFactors = FALSE)
str(mvt)

mvt$Date = strptime(mvt$Date, format = '%m/%d/%y %H:%M')
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

weekdayCounts = as.data.frame(table(mvt$Weekday))
str(weekdayCounts)

#Line plot
library(ggplot2)
ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))
weekdayCounts$Var1 = factor(weekdayCounts$Var1, ordered = TRUE, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))
ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1)) + xlab('Day of the week') + ylab('Total Motor Theft')

#Quiz
ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), linetype = 2) + xlab('Day of the week') + 
        ylab('Total Motor Vehicle Theft')

ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), alpha = 0.3) + xlab('Day of the week') + 
        ylab('Total Motor Vehicle Theft')
##

#Heatmaps
dayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(dayHourCounts)
dayHourCounts$Hour = as.numeric(as.character(dayHourCounts$Var2))
str(dayHourCounts)

ggplot(dayHourCounts, aes (x = Hour, y = Freq)) + geom_line(aes(group = Var1))

#add colors by days
ggplot(dayHourCounts, aes (x = Hour, y = Freq, col = Var1)) + geom_line(aes(group = Var1)) #OR
ggplot(dayHourCounts, aes (x = Hour, y = Freq)) + geom_line(aes(group = Var1, color = Var1), size = 2)

dayHourCounts$Var1 = factor(dayHourCounts$Var1, ordered = TRUE, levels = c('Monday', 'Tuesday', 'Wednesday', 
                                                                           'Thursday', 'Friday', 'Saturday', 
                                                                           'Sunday'))
ggplot(dayHourCounts, aes(x = Hour, y= Var1)) + geom_tile(aes(fill = Freq))
ggplot(dayHourCounts, aes(x = Hour, y= Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total
        MV Thefts') + theme (axis.title.y = element_blank())
ggplot(dayHourCounts, aes(x = Hour, y= Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = 'Total
        MV Thefts', low = 'white', high = 'black') + theme (axis.title.y = element_blank())

#Geo Maps
install.packages('maps')
install.packages('ggmap')
library(maps)
library(ggmap)

chicago = get_map(location = 'chicago', zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data = mvt [1:100, ], aes(x = Longitude, y = Latitude))
latlongCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(latlongCounts)
latlongCounts$Long = as.numeric(as.character(latlongCounts$Var1))
latlongCounts$Lat = as.numeric(as.character(latlongCounts$Var2))
ggmap(chicago) + geom_point(data = latlongCounts, aes (x = Long, y = Lat, color = Freq, size = Freq))
ggmap(chicago) + geom_point(data = latlongCounts, aes (x = Long, y = Lat, color = Freq, size = Freq)) + 
        scale_color_gradient(low = 'yellow', high = 'red')
ggmap(chicago) + geom_tile(data = latlongCounts, aes (x = Long, y = Lat, alpha = Freq), fill = 'red')

#Quiz
latlongCounts2 = subset(latlongCounts, Freq > 0)
ggmap(chicago) + geom_tile(data = latlongCounts2, aes (x = Long, y = Lat, alpha = Freq), fill = 'red')
nrow(latlongCounts) - nrow(latlongCounts2)
#