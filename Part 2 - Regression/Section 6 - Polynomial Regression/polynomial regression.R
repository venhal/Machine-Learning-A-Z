#Polynomial Regression
#importing the dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[,2:3]

#we need't to split the dataset into the training set and test set because
#this dataset is too small
#we needn't feature scaling, too. R will take this step auto in linear regression
#also in polynomial regression

#fitting Linear regression to the dataset
lin_reg = lm(formula = Salary ~., 
             data = dataset)
#fitting polynomial regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~.,
              data = dataset)

#visualising the Linear Regression results
library(ggplot2)
ggplot(dataset) + 
  geom_point(aes(x = Level, y = Salary, col = 'blue')) +
  geom_line(aes(x = Level, y = predict(lin_reg, newdata = dataset), col = 'red')) +
  xlab('Level') +
  ylab('Salary') 
#visualing the Polynomial regression results
ggplot(dataset) + 
  geom_point(aes(x = Level, y = Salary, col = 'blue')) + 
  geom_line(aes(x = Level, y = predict(poly_reg, newdata = dataset), col = 'red')) + 
  xlab('Level') +
  ylab('Salary')

#predicting a new result with linear regression
#you can see the difference between this part with last part in 'newdata', this is because we don't have 6.5 in our dataset
y_pred = predict(lin_reg, data.frame(Level = 6.5))

#predicting a new result with polynomial regression
y_pred = predict(poly_reg, data.frame(Level = 6.5,
                                      Level2 = 6.5^2,
                                      Level3 = 6.5^3,
                                      Level4 = 6.5^4))