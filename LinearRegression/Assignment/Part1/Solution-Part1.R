getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week2/Assignment/Part1')
getwd()
climateChange = read.csv('climate_change.csv')
str(climateChange)
summary(climateChange)

train = climateChange$Year <= 2006
climateTrain = climateChange[train,]
str(climateTrain)
climateTest = climateChange[!train, ]
str(climateTest)

#1.1 & 1.2
linModel1 = lm(Temp ~ .-(Year + Month), data = climateTrain)
summary(linModel1)

#2.2
cor(climateTrain[ , c("MEI", "CO2", "CH4", "N2O", "CFC.11", "CFC.12", "TSI", "Aerosols")])

#2.3
linModel2 = lm(Temp ~ (MEI + TSI + Aerosols + N2O), data = climateTrain)
summary(linModel2)
summary(linModel1)

#4
stepModel = step(linModel1)
summary(stepModel)

#5
modelPred = predict(stepModel, newdata = climateTest)
SSE = sum((modelPred - climateTest$Temp) ^ 2)
SST = sum((mean(climateTrain$Temp) - climateTest$Temp) ^ 2)
R2 = 1 - (SSE / SST)
