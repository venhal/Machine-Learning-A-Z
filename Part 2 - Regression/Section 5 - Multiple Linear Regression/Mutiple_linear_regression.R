#importing data
dataset <-  read.csv('50_Startups.csv')
dataset$State <- factor(dataset$State, 
                        levels = c('New York', 'California', 'Florida'), 
                        labels = c(1, 2, 3))

library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#feature scaling
#training_set <- scale(training_set)
#test_set <- scale(test_set)

#Fitting Multiple Linear Regression to the Training set
regressor <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = training_set)
#如果fit的多元回归用到了所有的自变量，可以将其用(Profit ～ . )来代替,此处根据fit的结果，发现只有R.D.Spend是相关的，所以可以将式子修改成简单线性回归
，即 Profit ~ R.D.Spend

#predicting the Test set results
y_pred <-  predict(regressor, newdata = test_set)

#Building the optimal model using backward elimination(使用反向淘汰的方法调整模型)
regressor <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = training_set)
summary(regressor)
#delete two data which near to 1
regressor <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend , data = training_set)
summary(regressor)
#delete the data which is 0.6
regressor <- lm(Profit ~ R.D.Spend + Marketing.Spend , data = training_set)
summary(regressor)
#delete the data which is bigger than 0.05
regressor <- lm(Profit ~ R.D.Spend, data = training_set)
summary(regressor)
#complete