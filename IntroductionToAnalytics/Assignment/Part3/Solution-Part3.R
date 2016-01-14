getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week1/Assignment/Part3')
getwd()
cps = read.csv('CPSData.csv')
str(cps)
summary(cps)

#1.2
summary(cps$Industry)

#1.3
sort(table(cps$State)) 

#1.4
table(cps$Citizenship)
(116639 + 7073) / nrow(cps)

#1.5
table(cps$Race, cps$Hispanic)

#2.1
summary(cps)

#2.2
table(cps$Age, is.na(cps$Married))

#2.3
table(cps$State, is.na(cps$MetroAreaCode))

#2.4
regMetro = table(cps$Region, is.na(cps$MetroAreaCode))
prop.table(regMetro)

#2.5
sort(tapply(is.na(cps$MetroAreaCode), cps$State, mean))
prop.table(table(is.na(cps$MetroAreaCode), cps$State))

#3.1
metroAreaCodes = read.csv('MetroAreaCodes.csv')
summary(metroAreaCodes)
str(metroAreaCodes)

countryCodes = read.csv('CountryCodes.csv')
summary(countryCodes)
str(countryCodes)

#3.2
cps = merge(cps, metroAreaCodes, by.x = 'MetroAreaCode', by.y = 'Code', all.x = TRUE)
str(cps)
summary(cps)
names(cps)

#3.3
nrow(cps[cps$MetroArea == 'Atlanta-Sandy Springs-Marietta, GA', ])
nrow(cps[cps$MetroArea == 'Baltimore-Towson, MD', ])
nrow(cps[cps$MetroArea == 'Boston-Cambridge-Quincy, MA-NH', ])
nrow(cps[cps$MetroArea == 'San Francisco-Oakland-Fremont, CA', ])

#3.4
sort(tapply(cps$Hispanic, cps$MetroArea, mean))

#3.5
sort(tapply(cps$Race == 'Asian', cps$MetroArea, mean))

#3.6
sort(tapply(cps$Education == "No high school diploma", cps$MetroArea, mean, na.rm = TRUE))

#4.1
cps = merge(cps, countryCodes, by.x = 'CountryOfBirthCode', by.y = 'Code', all.x = TRUE)
str(cps)
summary(cps)
names(cps)

#4.2
sort(table(cps$Country))

#4.3
tapply(cps$Country != 'United States', cps$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA', mean, na.rm = TRUE)

#4.4
indiaSub = subset(cps, cps$Country == 'India')
sort(table(indiaSub$MetroArea))

brazilSub = subset(cps, cps$Country == 'Brazil')
sort(table(brazilSub$MetroArea))

somaliaSub = subset(cps, cps$Country == 'Somalia')
sort(table(somaliaSub$MetroArea))




