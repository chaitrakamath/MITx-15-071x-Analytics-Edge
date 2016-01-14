getwd()
setwd('/Users/admin/Documents/Coursera/AnalyticsEdge-edX/Week3/Assignment')
getwd()

#1.1
songs = read.csv('songs.csv')
str(songs)
nrow(subset(songs, year == 2010))

#1.2
nrow(subset(songs, artistname == 'Michael Jackson'))

#1.3
subset(songs, artistname == 'Michael Jackson' & Top10 == 1)[, 'songtitle']

#1.4
sort(unique(songs[, 'timesignature']))

sort(table(songs[, 'timesignature']))

#1.5
songs[which.max(songs$tempo), 'songtitle']

#2.1
songsTrain = subset(songs, year <= 2009)
str(songsTrain)
songsTest =  subset(songs, year > 2009)

#2.2
novars = c('year', 'songtitle', 'songID', 'artistID', 'artistname')
songsTrain = songsTrain[, !(names(songsTrain) %in% novars)]
songsTest = songsTest[, !(names(songsTest) %in% novars)]
str(songsTrain)
str(songsTest)

songsModel1 = glm (Top10 ~ ., data = songsTrain, family = binomial)
summary(songsModel1)

#3.1
cor(songsTrain$loudness, songsTrain$energy)

#3.2
songsModel2 = glm(Top10 ~ . - loudness, data = songsTrain, family = binomial)
summary(songsModel2)
summary(songsModel1)

#3.3
songsModel3 = glm (Top10 ~ .-energy, data = songsTrain, family = binomial)
summary(songsModel3)
summary(songsModel1)

#4.1
testPredict = predict(songsModel3, newdata = songsTest, type = 'response')
head(testPredict)
table(songsTest$Top10, testPredict > 0.45)
accuracy = (309 + 19) / (309 + 5 + 40 + 19)

#4.2
table(songsTest$Top10)
314 / (314 + 59)

#4.3
table(songsTest$Top10, testPredict > 0.45)

#4.4
sensitivity = 19 / (19 + 40)
specificity = 309 / (309 + 5)
