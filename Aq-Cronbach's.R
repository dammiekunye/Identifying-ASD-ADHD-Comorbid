#install.packages("psych")
library("psych") #for Cronbach's alpha analysis
library("readr") #load the data
library("dplyr")

autism_spectrum <- read.csv("Answers-AQ.csv")


# Rename the columns in the dataset
names(autism_spectrum) <- 1:50

# Verify the column names have been changed
View(autism_spectrum)

# descriptive analysis
summary(autism_spectrum)

describe(autism_spectrum)

#Define item indices for autistic traits subscales
aq_social <- autism_spectrum[, c(1, 11, 13, 15, 22, 36, 44, 45, 47, 48)] 
aq_attention_switching <- autism_spectrum[, c(2, 4, 10, 16, 25, 32, 34,37,43,46)]
aq_attention_detail <- autism_spectrum[, c(5, 6, 9, 12, 19, 23, 28, 29, 30, 49)]
aq_communication <- autism_spectrum[, c(7, 17, 18, 26, 27, 31, 33, 35, 38, 39)]
aq_imagination <- autism_spectrum[, c(3, 8, 14, 20, 21, 24, 40, 41, 42, 50)]


#Perform Cronbach's alpha for each subscale to determine the most reliable subscale
alpha(aq_social)
alpha(aq_attention_switching)
alpha(aq_attention_detail)
alpha(aq_communication)
alpha(aq_imagination)
#alpha(aq_imagination, check.keys=TRUE) #this was run without the check.keys attribute,
#however, 4 items were negatively correlated with the first principal component and probably
#should be reversed indicating that they are negatived phrased and scored in the opposite 
#direction compared to other items

#NB: the Cronbach's alpha automatically identifies the negatively phrased items in the 
#analysis while Rasch model could not.

#the reliability values for the subscales are between 0.5 and 0.79 although this is considered as moderate
#but it is not a good value for psychologically test analysis. However, increasing
#the number of items might improve the value which leads me to computing the Cronbach's
#as a whole rather than in subscales

#Cronbach's alpha analysis for all autism spectrum items AQ-50
alpha(autism_spectrum)
alpha_results <- alpha(autism_spectrum, check.keys = TRUE)#this was run without the check.keys attribute,
#however, 4 items were negatively correlated with the first principal component and probably
#should be reversed. including the check.keys attribute improve the reliability by a value
#of 0.01
print(alpha_results)

# Check which items were reversed
print(alpha_results$keys)

# Extract item names and their signs from the alpha results
item_names <- colnames(alpha_results$scores)
item_names

# Identify negatively phrased items
negatively_phrased_items <- item_names[alpha_results$keys < 0]
