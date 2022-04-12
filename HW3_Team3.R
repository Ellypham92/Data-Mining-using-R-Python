#Home Work:3
#Problem Description: Write an R script to: 
# Read-in the titanic data (titanic_traning.csv).  
# Identify the severity of the missing value problem and data inconsistency problem. Specifically, generate
# a summary of missing values and inconsistent values for each of the features.

# List/Display all the records with missing values and/or inconsistent values. 
# Handle the missing values. 
# Correct the data inconsistency issue. 
# Store the clean data in a CSV file. 

#Inputs: Data file on titanic data 
#Output file: Clean data in a CSV file. 
#===============================================================================


#1. Read-in the titanic data (titanic_traning.csv).
###################################################
titanic.data <- read.csv("titanic_traning.csv")

#Explore the data
str(titanic.data)

# The data set has 9 attributes and 916 observations.
# The feature ID is nominal (though it's marked as integer) 
# The features age, sibsp, parch and fare are numeric
# The features sex and embarked are categorical
# The features pclass and survived are categorical (though it's marked as integer) 

#2. Data Summary 
################
# Summary of numeric variables
summary(titanic.data[c("age", "sibsp", "parch", "fare")]) 

#sibsp:	# of siblings / spouses aboard the Titanic	
#parch:	# of parents / children aboard the Titanic 


#Summary of categorical variable: pclass (Ticket class: 1 = 1st, 2 = 2nd, 3 = 3rd)
titanic.data$pclass <- as.factor(titanic.data$pclass)
summary(titanic.data$pclass)

# Summary of categorical variable: sex (Sex: Male, Female) )
titanic.data$sex <- as.factor(titanic.data$sex)
summary(titanic.data$sex)

#Summary of categorical variable: embarked (Port of Embarkation: C = Cherbourg, Q = Queenstown, S = Southampton)
titanic.data$embarked <- as.factor(titanic.data$embarked)
summary(titanic.data$embarked)

#Summary of categorical variable: survived (Survival: 0 = No, 1 = Yes)
titanic.data$survived <- as.factor(titanic.data$survived)
summary(titanic.data$survived)

#3. Summary of Missing Values and Inconsistent Values
#####################################################

# Create an empty summary table
num_of_Records <- nrow(titanic.data)
num_of_Features <- ncol(titanic.data) - 1

data.summary <- as.data.frame(matrix(nrow = num_of_Features, ncol = 5))

# Add table headings
colnames(data.summary) <- c("Features", "Missing Values", "% of Missing Values",
                            "Inconsistent Values", "% of Inconsistent Values")
# Add feature names in the first column
data.summary[,1] <- colnames(titanic.data[,-1])

# A. Compute missing values per feature
for (i in 1:num_of_Features) {
  data.summary[i,2] <- sum(is.na(titanic.data[,i]))
  data.summary[i,3] <- (sum(is.na(titanic.data[,i]))/num_of_Records)*100
}
# There are 188 missing values (20.524%) for the feature, age and 9 missing values (0.983%) for the feature, fare.


#B. Compute inconsistency in the data
#pclass feature --> No inconsistency
table(titanic.data$pclass) 

#age feature --> No inconsistency
table(titanic.data$age)

#sibsp feature --> No inconsistency
table(titanic.data$sibsp)

#parch feature --> No inconsistency
table(titanic.data$parch)

#fare feature --> No inconsistency
table(titanic.data$fare)

#survived feature --> No inconsistency
table(titanic.data$survived)

# pclass feature --> No inconsistency
table(titanic.data$pclass)

# sex feature --> Data inconsistency, as entries 'Female' instead of female and 'Male' instead of male.
table(titanic.data$sex)

# embarked feature --> Data inconsistency, as 'Queenstown' instead of Q.
table(titanic.data$embarked)


#Need to count data inconsistency for sex
inconsistent.count_sex = num_of_Records
for(i in 1:num_of_Records) {
  if((titanic.data$sex[i] == "female") || (titanic.data$sex[i] == "male")) {
    inconsistent.count_sex = inconsistent.count_sex - 1
  }
}
row_id <- which(data.summary$Features == "sex")
data.summary[row_id,4] <- inconsistent.count_sex
data.summary[row_id,5] <- (inconsistent.count_sex/num_of_Records)*100
View(data.summary)

#Need to count data inconsistency for embarked
inconsistent.count_emb = num_of_Records
for(i in 1:num_of_Records) {
  if((titanic.data$embarked[i] == "Q") || (titanic.data$embarked[i] == "C") || (titanic.data$embarked[i] == "S")) {
    inconsistent.count_emb = inconsistent.count_emb - 1
  }
}
row_id <- which(data.summary$Features == "embarked")
data.summary[row_id,4] <- inconsistent.count_emb
data.summary[row_id,5] <- (inconsistent.count_emb/num_of_Records)*100
View(data.summary)


# 4. List/Display all the records with missing values and/or inconsistent values.
#################################################################################
library("tidyverse")
missing_age <- titanic.data %>% filter(is.na(titanic.data$age))
missing_fare <- titanic.data %>% filter(is.na(titanic.data$fare))
inconsistent_sex <- titanic.data %>% filter(!(sex == 'male' | sex == 'female'))
inconsistent_emb <- titanic.data %>% filter(!(embarked == 'C' | embarked == 'Q'| embarked == 'S'))

missing_inconsistent_records <- rbind(missing_age, missing_fare, inconsistent_sex, inconsistent_emb)
print("List of all the records with missing values and/or inconsistent values")
missing_inconsistent_records

#Check if the missing/inconsistent records shown below are correct by matching the count from data.summary. 
nrow(rbind(missing_age, missing_fare, inconsistent_sex, inconsistent_emb))
# Yes, the below list is correct since it is matching with the total number of 204


#5. Handle the missing values and data inconsistency. 
#####################################################

# Let's correct the data inconsistency issues first in the attributes, sex and embarked
#######################################################################################
#feature - sex
for (i in 1:num_of_Records) {
  if(titanic.data$sex[i] == "Female") {
    titanic.data$sex[i] = "female"
  }
  else if (titanic.data$sex[i] == "Male") {
    titanic.data$sex[i] = "male"
  }
}

#feature - embarked
for(i in 1:num_of_Records) {
  if(titanic.data$embarked[i] == "Queenstown") {
    titanic.data$embarked[i] = "Q"
  }
}

# Recheck the sex feature --> No inconsistency
table(titanic.data$sex)

# Recheck the embarked feature --> No inconsistency
table(titanic.data$embarked)

# Handle the missing values
###########################
# impute missing info for Age
#############################
#install.package("ggplot2") # Install ggplot package
library("ggplot2")

#Plot the histogram for the feature age
ggplot(data = titanic.data) +
  geom_histogram(mapping = aes(x = age), binwidth = 5)

# Histogram appears to be normally distributed and hence mean of age, grouped by sex may be used to handle missing values.
titanic.data %>% group_by(sex) %>% summarise(mean = mean(age, na.rm = TRUE)) # checking the mean age, grouped by sex

#replace the Null values with mean age grouped by sex

titanic.data <- titanic.data %>% 
  group_by(sex) %>% 
  mutate(age = ifelse(is.na(age), 
                            mean(age, na.rm = TRUE),
                            age))

# Impute missing info for fare
##############################
# Plot the histogram for the feature fare
ggplot(data = titanic.data) +
  geom_histogram(mapping = aes(x = fare), binwidth = 20)
# Histogram appears to be skewed and hence median of fare, grouped by pclass may be used handle missing values.
titanic.data %>% group_by(pclass) %>% summarise(median = median(fare, na.rm = TRUE)) #check the median fare, grouped by pclass

#Replace the Null values with median fare grouped by pclass
titanic.data <- titanic.data %>%
  group_by(pclass) %>%
  mutate(fare = ifelse(is.na(fare),
                       median(fare, na.rm = TRUE),
                       fare))


# 6. Store the clean data in a CSV file. 
########################################
write.csv(file = "HW3_Team3_titanic_training_cleaned.csv", titanic.data, row.names = FALSE)



