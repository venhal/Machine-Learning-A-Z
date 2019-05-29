#importing data
dataset <-  read.csv('Data.csv')
library(caTools)
set.seed(123)
split <- sample.split(dataset$purchase, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#feature scaling
training_set <- scale(training_set)
test_set <- scale(test_set)