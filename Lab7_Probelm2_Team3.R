#Lab:7b 
#Forecasting Numeric Data: Regression Tree and Model Tree
#Team:3
#Team Members: 
# Member-1: Denish Shrestha 33.33% contribution
# Member-2: Elly Pham       33.33% contribution
# Member-3: Benoy Thomas    33.33% contribution

#Submission Date: 03/20/2022

#Problem Description: Predicting wine quality
#Inputs:              Wine data
#Output file:         Display values on the Console (R file)
#===============================================================================

#UCI Machine Learning Repository:  https://archive.ics.uci.edu/ml/datasets/wine+quality 

#File: whitewines.csv
#Examples: 4338
#Dependent variable (y): Quality
#Independent variables (X): Column 1 to Column 11

#################################################
#1.Collect data
#################################################
wine <- read.csv("whitewines.csv")

#Explore the data
str(wine)

# the distribution of quality ratings (Normal distribution)
hist(wine$quality)

# summary statistics of the wine data (All variables are numeric.)
summary(wine)


#################################################
#2. Preprocess the data
#################################################
wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

#################################################
#3. Train a model on training data
#################################################
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)

#get basic information about the tree
m.rpart

#get the detailed information about the tree
summary(m.rpart)


# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(m.rpart, digits = 3)

#few adjustment to the diagram
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra =101 )
#The most important attribute is alcohol.


#################################################
#4. Evaluate model performance
#################################################
#generate predictions for the testing dataset
p.rpart <- predict(m.rpart, wine_test)

#compare the correlation
cor(p.rpart, wine_test$quality)
#The Correlation between the predicted and the actual value is "acceptable".

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, wine_test$quality)


#################################################
#5. Improving model performance
#################################################
# Let's try Model Tree --> Much more complex approach
#The current state-of-the-art in model trees is the Cubist algorithm,


# train a Cubist Model Tree
#install.packages("Cubist")
library(Cubist)
m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)

# display basic information about the model tree
m.cubist

# display the tree itself
summary(m.cubist)

# generate predictions for the model
p.cubist <- predict(m.cubist, wine_test)

# summary statistics about the predictions
summary(p.cubist)


# correlation between the predicted and true values
cor(p.cubist, wine_test$quality)
#The correlation between the predicted and the actual value is 0.62 which is
# comparatively better than the previous model.

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(wine_test$quality, p.cubist)  
# The MAE also reduced to 0.53 from 0.58.