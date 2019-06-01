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

#fitting logistic regression to the training set
classifier = glm(formula = Purchased ~., 
                 family = binomial, 
                 data = training_set)

#predicting the test set result
prob_pred = predict(classifier, type = 'response', newdata = testing_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#making the confusion Matrix
cm = table(testing_set[ , 3], y_pred)

#visualising the training set result











