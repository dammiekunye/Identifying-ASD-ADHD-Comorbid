
library("psych") # this library is used for computing the Cronbach's alpha
library("dplyr")

adult_ADHD <- read.csv("Answers-ASRS.csv")
adult_ADHD

# Drop column ID
# Remove the non-numeric identifier column
adult_ADHD <- adult_ADHD[, -1]
print(adult_ADHD)

# Rename the columns in the dataset
names(adult_ADHD) <- 1:18

# Verify the column names have been changed
View(adult_ADHD)

# descriptive analysis
summary(adult_ADHD)

#Define item indices for ADHD traits subscales
#the subscales include inattentive items and Hyperactive impulsive items
asrs_inattentive <- adult_ADHD[, c(1, 2, 3, 4, 5, 6, 7, 8, 9)] 
asrs_hyperactive_impulsive <- adult_ADHD[, c(10, 11, 12, 13, 14, 15, 16, 17, 18)]

# Ensure that all values are numeric
asrs_inattentive <- as.data.frame(lapply(asrs_inattentive, as.numeric))
asrs_hperactive_impulsive <- as.data.frame(lapply(asrs_hperactive_impulsive, as.numeric))

#perform Cronbach's alpha on each subscale 
alpha(asrs_inattentive) 
alpha(asrs_hperactive_impulsive)

#the reliability value for both subscales is between 0.77 and 0.79 which is quite close to 0.8 indictaing an acceptable 
#reliability value

alpha(adult_ADHD)# the reliability value is 0.86 indicating a good reliability value 

