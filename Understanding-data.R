#Problem Description: Provide Summary of the features, relationship between the features
#                     through visualization and comments on the patterns that you observe
#                     from the descriptive statistics and visualizations
#Inputs:              Used Cars data 
#Output file:         Display values on the Console 
#========================================
#NOTE: Observations and comments are written below the respective code. 


#1.Read-in the used cars data (usedcars.csv).
#################################################
used.cars <- read.csv("usedcars.csv")

#Explore the data 
str(used.cars)
#The dataset contains total 150 observations with 6 features for each variable.
#The name of the features are year, model, price, mileage, color and transmission.
#Numeric variables are year, price, mileage.
#Categorical variables are model, color, transmission.

#2. Data Summary of the features
#################################################

#Summary of numeric variables
summary(used.cars$year)
#The minimum and maximum year of used cars are Year 2000 and 2012 respectively.
#The median and mean of the car is the same ie. year 2009.
#The difference between the minimum value and Median is higher than the difference
#   between the maximum value and Median suggesting data are spread out more towards
#   older cars. Hence, The data set contains high density cars for year 2009 - 2012.

summary(used.cars$price)
#The price of the cars ranges from $3,800 to $21,992 and the mean price is $12,962.
#The difference between the values of the mean and the median is not large. 
#Difference between Q1 and Q3 is around $4,000, which means the lower and the upper 
#   25% of the value for the price is more widely distributed than the middle 50%.


summary(used.cars$mileage)
#The value of mean for the mileage is greater than median by roughly 8000 miles.
#Similarly, the difference between the minimum value and Q1 is less than the maximum 
#   value and Q3. 
#As a result, we may assume that the values for the mileage is right skewed.
#In other words, the number of cars with higher values of mileage are more spread out.

#Summary of categorical variable: model
table(used.cars$model)
temp = table(used.cars$model)
print("The mode of the variable is")
temp[which.max(table(used.cars$model))]
#There are 3 models of cars available. 78 cars are SE, 23 are SEL and 49 are SES.


#Summary of categorical variable: color
table(used.cars$color)
temp = table(used.cars$color)
print("The mode of the variable is")
temp[which.max(table(used.cars$color))]
#Customers can choose the color of the car from 9 different values.
#Cars with colors Gold(1), Green(5) and Yellow(3) are significantly lesser in numbers. 

#Summary of categorical variable: transmission
table(used.cars$transmission)
temp = table(used.cars$transmission)
print("The mode of the variable is")
temp[which.max(table(used.cars$transmission))]
#128 cars are available in auto transmission whereas 22 cars are available in manual.

#3. Data Visualization - EDA
#################################################

#Install and load necessary package
#install.packages("ggplot2")
library("ggplot2")

########## A. Boxplot Analysis ##################

#Price grouped by categorical variables
########
ggplot(used.cars, aes(x = transmission, y = price))+
  geom_boxplot()
#The plot shows 4 outliers for the cars with auto transmission. 
#Although the price range for middle 50% for both category are close,the number of cars with auto 
#   transmission are more spread out than the cars with manual transmission.
#In other words, there are more cars with the prices that falls under lower 25% 
#   and the upper 25% category than in the middle 50% with auto transmission.

ggplot(used.cars, aes(x = reorder(color, price, FUN = median), y = price)) +
  geom_boxplot()
#If we order the data based on the median price for each color, we can see that the median price of black 
#and silver color is higher than the others. 
#Cars with Red Color has one outlier that is below 5k price point (the extreme low price).
#Yellow colored cars have the widest range in term of price compared to others.

ggplot(used.cars, aes(x = model, y = price))+
  geom_boxplot()

ggplot(used.cars, aes(x = as.factor(year), y = price))+
  geom_boxplot()
#The plot suggests that the price of the cars are increasing with the increase in year.
#Cars with year 2011 seems to have 3 outliers with the highest prices.
#Cars with year 2006 seems unusual in a way that one outlier is present at the lower end 
#and it is responsible to change mainly the median and Q1.

# Price vs transmission vs color vs model
ggplot(used.cars, aes(x = transmission, y = price))+
  geom_boxplot()+
  facet_grid(model ~ color)


#Mileage grouped by categorical variables  
########
ggplot(used.cars, aes(x = transmission, y = mileage))+
  geom_boxplot()

ggplot(used.cars, aes(x = color, y = mileage))+
  geom_boxplot()

ggplot(used.cars, aes(x = model, y = mileage))+
  geom_boxplot()

ggplot(used.cars, aes(x = as.factor(year), y = mileage))+
  geom_boxplot()

## Mileage vs transmission vs color vs model
ggplot(used.cars, aes(x = transmission, y = mileage))+
  geom_boxplot()+
  facet_grid(model ~ color)

########## B. Scatterplot Analysis ##################
ggplot(data = used.cars)+ 
  geom_point(mapping = aes(x= mileage, y = price))

ggplot(data = used.cars)+
  geom_point(mapping = aes(x = mileage, y = price, color = transmission))

ggplot(data = used.cars)+
  geom_point(mapping = aes(x = mileage, y = price, color = transmission))+
  facet_grid(~model)

#The scatterplot suggests that the car prices for SEL model are higher than 
#   the rest of the other 2 models. However, the number of cars available and 
#   the mileage of SEL model cars are lower.
#The scatterplot also suggests that there is a negative relationship between price
#   and the mileage for all car models. 
#We can make an assumption here that the the lower mileage and the lower inventory level
#   of SEL model cars might be the factors that are affecting the price of this
#   particular model. 


########## C. Histogram Analysis ##################

#Price 
######
ggplot(data = used.cars)+
  geom_histogram(mapping = aes(x = price), binwidth = 2000)

ggplot(data = used.cars)+
  geom_histogram(mapping = aes(x = price, color = transmission, fill = transmission),binwidth = 2000)

ggplot(data = used.cars)+
  geom_histogram(mapping = aes(x = price, color = model, fill = model), binwidth = 2000)

#All the histograms show that the data is not normally distributed and are skewed.
#Hence, we will use median value as the measure of central of tendency. 
#SE model cars accounted more than other models in the data set. 
#The most frequency of the price interval is in between $12500 to $15000.

#Mileage
########

ggplot(data = used.cars)+
  geom_histogram(mapping = aes(x = mileage), binwidth = 20000)

ggplot(data = used.cars)+
  geom_histogram(mapping = aes(x = mileage, color = transmission, fill = transmission), binwidth = 20000)

ggplot(data = used.cars)+
  geom_histogram(mapping = aes(x = mileage, color = model, fill = model), binwidth = 20000)

#All the histograms show that the data is not normally distributed and are skewed. 
# Hence, median is a better measure than mean for this data because median is resistant to outliers.
# The number of cars with SE model is comparatively higher than the other models (SEL, SES).
# There are some SE model cars with extreme mileage value (more than 150k miles).
