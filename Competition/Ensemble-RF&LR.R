#Averaging random forest and logistic regression predictions
predictions = (logPredict + rfPredict) / 2
head(predictions)
#This submission resulted in score of 0.92530 ******Latest
mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = predictions)
write.csv(mySubmission, 'Ensemble_RF-LRv.csv', row.names = FALSE)

#Averaging SVM and random forest predictions
predictions = (rfPredict + svmPredictions) / 2
head(predictions)
#Submission did not result in better score compared to one above
mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = predictions)
write.csv(mySubmission, 'Ensemble_RF-SVM.csv', row.names = FALSE)

#Adding higher weights to random forest predictions
predictions = (logPredict + 9 * rfPredict) / 10
head(predictions)
#Submission did not result in better score compared to one above
mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = predictions)
write.csv(mySubmission, 'Ensemble_RF-LRv2.csv', row.names = FALSE)

#Adding higher weights to logistic regression model predictions
predictions = (3 * logPredict + 7 * rfPredict) / 10
head(predictions)
#Submission did not result in better score compared to one above
mySubmission = data.frame(UniqueID = nyTest$UniqueID, Probability1 = predictions)
write.csv(mySubmission, 'Ensemble_RF-LRv3.csv', row.names = FALSE)
