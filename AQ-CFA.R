#install.packages("foreign", dependencies=TRUE)
#install.packages("lavaan", dependencies=TRUE) 

library(foreign) 
library(lavaan)


autism_spectrum <- read.csv("Answers-AQ.csv")

View(autism_spectrum)


#Define item indices for autistic traits subscales
aq_social <- autism_spectrum[, c(1, 11, 13, 15, 22, 36, 44, 45, 47, 48)] 
aq_attention_switching <- autism_spectrum[, c(2, 4, 10, 16, 25, 32, 34,37,43,46)]
aq_attention_detail <- autism_spectrum[, c(5, 6, 9, 12, 19, 23, 28, 29, 30, 49)]
aq_communication <- autism_spectrum[, c(7, 17, 18, 26, 27, 31, 33, 35, 38, 39)]
aq_imagination <- autism_spectrum[, c(3, 8, 14, 20, 21, 24, 40, 41, 42, 50)]

corr_table <- round(cor(autism_spectrum[,1:50]),2)
View(corr_table)
#A variance-covariance matrix has diagonals for variances and off-diagonals 
#for covariances. A correlation table interprets standardized covariances 
#between items, with diagonal elements always one due to item perfect correlation.

# Define the model specification for 50 items
items <- paste0("AQ", sprintf("%02d", 1:50))
model_spec <- paste("f1 =~", paste(items, collapse = " + "))

# Perform CFA
onefac50items <- cfa(model_spec, data=autism_spectrum, std.lv=TRUE)

# Summarize the results
summary(onefac50items, fit.measures=TRUE, standardized=TRUE)

#fit statistics
summary(onefac50items, fit.measures=TRUE, standardized=TRUE)
#the results from the one-factor analysis indicate that the model might be a poor fit 
#and looking at the standardized loadings we report to the Principal Investigator that the SAQ-8 as it stands does not possess good psychometric properties
#a multi-item factor analysis is suggested to improve the model and check the 
#correlation among the subscales which is our objective 

# Define the model specification for 5 factors with 50 items
#To compute a model in lavaan, the user first needs to specify 
#the model structure as text, using a few pre-defined commands
#the laavan command, =~ defines the reflective latent variable representing
#a factor
subscale_model <- '
  social =~ AQ01 + AQ11 + AQ13 + AQ15 + AQ22 + AQ36 + AQ44 + AQ45 + AQ47 + AQ48
  attention_switching =~ AQ02 + AQ04 + AQ10 + AQ16 + AQ25 + AQ32 + AQ34 + AQ37 + AQ43 + AQ46
  attention_detail =~ AQ05 + AQ05 + AQ09 + AQ12 + AQ19 + AQ23 + AQ28 + AQ29 + AQ30 + AQ49
  communication =~ AQ07 + AQ17 + AQ18 + AQ26 + AQ27 + AQ31 + AQ33 + AQ35 + AQ38 + AQ39
 imagination =~ AQ03 + AQ08 + AQ14 + AQ20 + AQ21 + AQ24 + AQ40 + AQ41 + AQ42 + AQ50
'

# Perform CFA
fivefac50items <- cfa(subscale_model, data=autism_spectrum, std.lv=TRUE)

# Summarize the results with fit measures and standardized estimates
summary(fivefac50items, fit.measures=TRUE, standardized=TRUE)



adult_ADHD <- read.csv("Answers-ASRS.csv")
adult_ADHD

# Drop column ID
# Remove the non-numeric identifier column
adult_ADHD <- adult_ADHD[, -1]
print(adult_ADHD)


View(adult_ADHD)

#Define item indices for ADHD traits subscales
#the subscales include inattentive items and Hyperactive impulsive items
asrs_inattentive <- adult_ADHD[, c(1, 2, 3, 4, 5, 6, 7, 8, 9)] 
asrs_hyperactive_impulsive <- adult_ADHD[, c(10, 11, 12, 13, 14, 15, 16, 17, 18)]



corr_table1 <- round(cor(adult_ADHD[,1:18]),2)
View(corr_table1)
#A variance-covariance matrix has diagonals for variances and off-diagonals 
#for covariances. A correlation table interprets standardized covariances 
#between items, with diagonal elements always one due to item perfect correlation.

# Define the model specification for 18 items
items1 <- paste0("ASRS", sprintf("%02d", 1:18))
model_spec1 <- paste("f1 =~", paste(items1, collapse = " + "))

# Perform CFA
onefac18items <- cfa(model_spec1, data=adult_ADHD, std.lv=TRUE)

# Summarize the results
summary(onefac18items, fit.measures=TRUE, standardized=TRUE)

#fit statistics
summary(onefac18items, fit.measures=TRUE, standardized=TRUE)
#the results from the one-factor analysis indicate that the model might be a poor fit 
#and looking at the standardized loadings we report to the Principal Investigator that the SAQ-8 as it stands does not possess good psychometric properties
#a multi-item factor analysis is suggested to improve the model and check the 
#correlation among the subscales which is our objective 

# Define the model specification for 5 factors with 50 items
subscale_model <- '
  inattentive =~ ASRS01 + ASRS02 + ASRS03 + ASRS04 + ASRS05 + ASRS06 + ASRS07 + ASRS08 + ASRS09
  hyperactive_impulsive =~ ASRS10 + ASRS11 + ASRS12 + ASRS13 + ASRS14 + ASRS15 + ASRS16 + ASRS17 + ASRS18
'

# Perform CFA
Twofac18items <- cfa(subscale_model, data=adult_ADHD, std.lv=TRUE)

# Summarize the results with fit measures and standardized estimates
summary(Twofac18items, fit.measures=TRUE, standardized=TRUE)

#combine the AQ and ASRS dataset to check the correlation between the subscales
#of both data 
# Combine datasets by binding columns
combined_data <- cbind(autism_spectrum, adult_ADHD)

# Define the model specification for combined CFA
combined_model <- '
  # AQ subscales
  social =~ AQ01 + AQ11 + AQ13 + AQ15 + AQ22 + AQ36 + AQ44 + AQ45 + AQ47 + AQ48
  attention_switching =~ AQ02 + AQ04 + AQ10 + AQ16 + AQ25 + AQ32 + AQ34 + AQ37 + AQ43 + AQ46
  attention_detail =~ AQ05 + AQ05 + AQ09 + AQ12 + AQ19 + AQ23 + AQ28 + AQ29 + AQ30 + AQ49
  communication =~ AQ07 + AQ17 + AQ18 + AQ26 + AQ27 + AQ31 + AQ33 + AQ35 + AQ38 + AQ39
  imagination =~ AQ03 + AQ08 + AQ14 + AQ20 + AQ21 + AQ24 + AQ40 + AQ41 + AQ42 + AQ50
  # ASRS subscales
  inattentive =~ ASRS01 + ASRS02 + ASRS03 + ASRS04 + ASRS05 + ASRS06 + ASRS07 + ASRS08 + ASRS09
  hyperactive_impulsive =~ ASRS10 + ASRS11 + ASRS12 + ASRS13 + ASRS14 + ASRS15 + ASRS16 + ASRS17 + ASRS18
'

# Perform CFA
combined_cfa <- cfa(combined_model, data = combined_data, std.lv = TRUE)

# Summarize the results
summary(combined_cfa, fit.measures = TRUE, standardized = TRUE)

