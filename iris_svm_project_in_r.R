#Here we will using Support Vector Machines for this project in R

#We will be using the iris dataset, which is inherent in R via the ISLR library
#Let's import the ISLR library
library(ISLR)

#Now let's create our dataframe for analysis
#There are 150 observations of five variables
irises = iris


#In order to use SVM, we must have the e1071 library, which we will import here
library(e1071)


#If you want to see all the things you can do with SVM in R you can use "help"
help("svm")


#Let's start building our SVM model
model = svm(Species ~ ., data = irises)

#Here's a summary of the model
#Cost is basically what allows the SVM to have a "soft" margin
#A small gamma means a Gaussian of large variance, so the influence of the Support Vector is larger. Vice versa of course
summary(model)


#Let's start tuning our results
#We are training a bunch of SVM models and seeing which one has the best fit
#Usually you should have a training and test set. We are running the model on the entire dataset in this example
#Ranges is where you put in the range of gamma and cost values
tuned_results <- tune(svm, train.x = irises[1:4], train.y = irises[,5], kernel = 'radial', ranges = list(cost = c(0.1, 1, 10), gamma = c(0.5, 1 ,2)))

#The summary reports back the best error performance for the combos of cost and gamma parameters
#The best combination was Cost = 1 and gamma = 0.5
summary(tuned_results)

#Now we will apply the cost and gamma parameters to our SVM model
tuned_svm = svm(Species ~ ., data = irises, kernel = 'radial', cost = 1, gamma = 0.5)

#Here is a summary of our tuned svm model
summary(tuned_svm)
