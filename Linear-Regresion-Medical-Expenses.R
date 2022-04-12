#Linear Regression
#Problem Description: Predicting medical expenses using linear regression
#Inputs:              Hypothetical medical expenses for patients in the US
#Output file:         Display values on the Console (R file)
#===============================================================================

#File: insurance.csv
#Examples: 1338
#Dependent variable (y): Medical expenses
#Independent variables (X): age, sex, BMI, children, smoker, region 

#################################################
#1.Collect data
#################################################
insurance <- read.csv("insurance.csv")

#################################################
#2.Explore the data
#################################################
str(insurance)

####Get feel of the data
hist(insurance$expenses)
#Expenses data is skewed to the right.

#Exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

#correlation matrix chart
pairs(insurance[c("age", "bmi", "children", "expenses")], main = "Correlation")

#correlation matrix chart using psych library
#install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")], main = "Correlation")

#Observations: 
#age and bmi: a weak positive correlation --> as someone ages, their body mass tends to increase. 
#age and expenses: a moderate positive correlation --> as age increase, the expected cost of insurance goes up.
#bmi and expenses: a moderate positive correlation --> as body mass, the expected cost of insurance goes up.
#children and expenses: a weak positive correlation --> as a number of children increase, the expected cost of insurance goes up.

#################################################
#3. Train a model on the data 
#################################################
ins_model <- lm(expenses ~ ., data = insurance)

#See estimated beta coefficients
ins_model

#################################################
#4. Evaluate the model performance 
#################################################
summary(ins_model)

#Observations: 
#1. Residuals section provides summary statistics for the errors in our predictions
#error = actual - predicted 

#2. Significance level: p value should be lower than 0.001, 0.05, or 0.1 to prove that the effect is significant and not just by chance
  # If we take 0.05 as the threshold, the p value for sexmale and regionnorthwest are more than 0.05 which indicates those variables are
    #not statistically significant.
  # Age,bmi children and smokeryes have positive correlation with the expense.
  # sexmale, regionsoutheast and regionsouthwest have negative correlation with the expense.

#3. Multiple R-squared value: Provides a measure of how well our model as a whole explains the values of the dependent variable.
#Here,Since the R-squared value is 0.7494 --> the model explains nearly 75% of the variation in the dependent variable 
#Adjusted R-squared value corrects R-squared by penalizing models with a large number of independent variables.

######Can the performance be improved further?###################

#################################################
#5. Improvising the model performance
#################################################

#1. Add a non-linear term for age  (cost increase as age increases)
insurance$age2 <- insurance$age^2
ins_model <- lm(expenses ~ ., data = insurance)
summary(ins_model)

#2. Create an indicator for obesity (BMI > 30 may cost more)
# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model <- lm(expenses ~ ., data = insurance)
summary(ins_model)
# The model performance increased slightly to 0.7564 after adding the indicator for obesity.


#3. Specified an interaction between obesity and smoking (interactions)
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + 
                  smoker*bmi30 + region, data = insurance)
summary(ins_model2)
#Insurance cost is the highest for individuals who are obese and smokers.
#The interaction between the smokeryes and bmi30 is statistically significant. (p value < 0.05)
#Also, the model performance increased comparatively more to 0.8653 after adding the interaction.


#Making prediction with the regression model
insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)
#Correlation between the actual and predicted data is quite strong.

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

#Insurance cost Prediction for this individual: 
#X = (age = 30, Children = 2, bmi = 30, sex = male, smoker = no, region = northeast)
predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))

#The predicted cost for this individual is $5973.77.
