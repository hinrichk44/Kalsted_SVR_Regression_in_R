#Here we are going to do a Support Vector Regression project


#Here we are importing our dataset
#We have 10 observations of three variables
#We have a person's position, level, and salary
salaries = read.csv('positions.csv')

#Here we will remove the Position column since it's redundant with Level
salaries =  salaries[2:3]


#Here we are creating our SVR regression model


#First you must install the e1071 package
#e1071 provides functions for statistic and probabilistic algorithms
#Including a fuzzy classifier, naive Bayes classifier, bagged clustering, short-time Fourier transform, support vector machine, etc.
#install.packages('e1071')

#Now we are importing the e1071 library
library(e1071)


#Here we will set up the official SVR regressor
#SVM can be used in many ways, you have to specify whether you want classification or regression in R
regressor = svm(formula = Salary ~ ., data = salaries, type = 'eps-regression')


#Here we will predict new results based on our SVR regressor
#So we are predicting a specific value based on the Level being 6.5
#Our result is a salary of $177,861
y_pred = predict(regressor, data.frame(Level = 6.5))



#Now we will visualize our SVR results

#First we will import the ggplot2 library
library(ggplot2)


#Here we will initialize the plot for our polynomial regression
ggplot() + 
  #Here we are stating that we are plotting points/observed variables with the color red
  geom_point(aes(x = salaries$Level, y = salaries$Salary), color = 'red') +
  #Here we are adding the predictions from the linear regression via a line plot
  #Remember that the salaries must be predicted so we use the predict function via our SVR regressor
  #The color of our prediction line will be blue
  geom_line(aes(x = salaries$Level, y = predict(regressor, newdata = salaries)), color = 'blue') +
  #Here is simple graph formatting with titles and axes
  ggtitle("Salaries vs. Level (SVR Regression") +
  xlab('Level') +
  ylab('Salary')