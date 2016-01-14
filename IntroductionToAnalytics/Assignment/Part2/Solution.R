getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week1/Assignment/Part2')
getwd()

#Read data
ibm = read.csv('IBMStock.csv')
ge = read.csv('GEStock.csv')
pg = read.csv('ProcterGambleStock.csv')
cocaCola = read.csv('CocaColaStock.csv')
boeing = read.csv('BoeingStock.csv')

#Format date
ibm$Date = as.Date (ibm$Date, '%m/%d/%y')
ge$Date = as.Date (ge$Date, '%m/%d/%y')
pg$Date = as.Date (pg$Date, '%m/%d/%y')
cocaCola$Date = as.Date (cocaCola$Date, '%m/%d/%y')
boeing$Date = as.Date (boeing$Date, '%m/%d/%y')

################Part1##################

#1.1
nrow(ibm)
nrow(ge)
nrow(pg)
nrow(cocaCola)
nrow(boeing)

#1.2
min(ibm$Date)
min(ge$Date)
min(pg$Date)
min(cocaCola$Date)
min(boeing$Date)

#1.3
max(ibm$Date)
max(ge$Date)
max(pg$Date)
max(cocaCola$Date)
max(boeing$Date)

#1.4
summary(ibm$StockPrice)

#1.5
summary(ge$StockPrice)

#1.6
summary(cocaCola$StockPrice)

#1.7
summary(boeing$StockPrice)

#1.8
sd(pg$StockPrice)

################Part2##################

#2.1
plot(cocaCola$StockPrice ~ cocaCola$Date, col = 'red' ,xlab = 'Stock Price', ylab = 'Time', type = 'l', main = 'Coca Cola Stock Price Trend')

#2.2 & 2.3
lines(pg$StockPrice ~ pg$Date, col = 'blue')
legend(x = 100, y = 5, legend = c('Coca Cola', 'Procter & Gamble'), col = c('red', 'blue'), lty = c(1,1))
abline(v = as.Date (c('2000-03-01')), lwd = 2)


####Part3####

plot(cocaCola$Date[301:432], cocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(pg$Date[301:432], pg$StockPrice[301:432], col = 'orange')
lines(boeing$Date[301:432], boeing$StockPrice[301:432], col = 'green')
lines(ge$Date[301:432], ge$StockPrice[301:432], col = 'blue')
lines(ibm$Date[301:432], ibm$StockPrice[301:432], col = 'black')
abline(v = as.Date (c('1997-09-01', '1997-11-01')), lwd = 2)
abline(v = as.Date(c('2004-01-01', '2005-12-31')), lwd = 2)


######Part4#####

#4.1
sort(tapply(ibm$StockPrice, months(ibm$Date), mean, na.rm = TRUE)) > mean(ibm$StockPrice)

#4.2
sort(tapply(ge$StockPrice, months(ge$Date), mean, na.rm = TRUE))
sort(tapply(cocaCola$StockPrice, months(cocaCola$Date), mean, na.rm = TRUE))

#4.3
sort(tapply(boeing$StockPrice, months(boeing$Date), mean, na.rm = TRUE))
sort(tapply(pg$StockPrice, months(pg$Date), mean, na.rm = TRUE))
