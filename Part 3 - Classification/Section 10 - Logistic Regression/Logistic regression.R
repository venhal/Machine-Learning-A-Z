#Logistic regression

#Importing the dataset
dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[, 3:5]

#splitting the dataset into the Training set and testing set
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

#feature scaling
training_set[, 1:2] = scale(training_set[, 1:2])
testing_set[, 1:2] = scale(testing_set[, 1:2])
