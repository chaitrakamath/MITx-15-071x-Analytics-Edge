getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week7/Lecture/')
getwd()

who = read.csv('WHO.csv')
str(who)
plot(who$FertilityRate ~ who$GNI)

install.packages('ggplot2')
library(ggplot2)
scatterplot = ggplot(who, aes (x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point(color = 'blue', size = 3, shape = 17)
scatterplot + geom_point(color = 'darkred', size = 3, shape = 8)
scatterplot + geom_point(color = 'darkred', size = 3, shape = 8) + ggtitle('Fertility Rate vs. Gross National Income')
fertilityGNIPlot = scatterplot + geom_point(color = 'darkred', size = 3, shape = 8) + ggtitle('Fertility Rate vs. Gross National Income')
pdf('FertilityGNIPlot.pdf')
print(fertilityGNIPlot)
dev.off()

#Quiz
scatterplot + geom_point(color = 'darkred', size = 3, shape = 15)
#

ggplot(who, aes(x = GNI, y = FertilityRate, col = Region)) + geom_point()
ggplot(who, aes(x = GNI, y = FertilityRate, col = LifeExpectancy)) + geom_point()
ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()

model = lm(Under15 ~ log(FertilityRate), data = who)
summary(model)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm')
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm', level = 0.99)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm', se = FALSE)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = 'lm', color = 'orange')

#Quiz
ggplot(who, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer (palette = 'Dark2')
##