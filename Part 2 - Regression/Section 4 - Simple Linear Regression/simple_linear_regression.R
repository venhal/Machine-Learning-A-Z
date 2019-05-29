#importing data
dataset <-  read.csv('Salary_Data.csv')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#feature scaling
#training_set <- scale(training_set)
#test_set <- scale(test_set)

#fit simple linear regression to the training set
regressor <- lm(formula = Salary ~ YearsExperience, data = training_set)


#Predict the Test set results
y_pred <- predict(regressor, newdata = test_set)

# Visualising the Training set result
install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary), 
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), 
             color = 'blue') +
  xlab ('YearExperience') +
  ylab ('Salary') +
  ggtitle ('Experience vs Salary (Training set)')
 
# Visualising the Test set result  
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
       color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  xlab('YearExperience') +
  ylab('Salary') +
  ggtitle('Experience vs Salary (Test set)')
  
