# Artificial Neural Networks(ANN)
# Importing the data
dataset = read.csv("Churn_Modelling.csv")
dataset = dataset[4:14]

# Encoding the categorical data as factor
dataset$Geography = factor(dataset$Geography,
                           levels = c('France', 'Spain', 'Germany'),
                           labels = c(1, 2, 3))
dataset$Gender = factor(dataset$Gender,
                        levels = c('Female', 'Male'),
                        labels = c(1, 2))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature scaling
training_set[-c(2, 3, 11)] = scale(training_set[-c(2, 3, 11)])
test_set[-c(2, 3, 11)] = scale(test_set[-c(2, 3, 11)])
#install.packages("h2o") 
library(h2o)
h2o.init(nthreads = -1)
classifer = h2o.deeplearning(y = 'Exited',
                             training_frame = as.h2o(training_set),
                             activation = 'Rectifier',
                             hiden = c(6, 6),
                             epochs = 100,
                             training_samples_per_iteration = -2)

# Pridicting the Test set results
pro_pred = h2o.predict(classifer, newdata = as.h2o(test_set[-11]))
y_pred = pro_pred > 0.5
y_pred = as.vector(y_pred)
