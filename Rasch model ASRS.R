library("TAM") #for for partial credit rasch model analysis(PCM)
library("WrightMap") # for plotting the variable map
library("Hmisc") # for descriptive data analysis
library("formattable") # for format number as percentage
library("readr") #load the data
library("dplyr") #to manipulate dataframes efficiently
library("eRm") #for improved person item maps
#install.packages("knitr")
library("knitr") #used to create tables 
#install.packages("tidyverse")
library("tidyverse")
library("ggplot2")
library("here")

adult_ADHD <- read.csv("Answers-ASRS.csv")
adult_ADHD

# Drop column ID
adult_ADHD <- adult_ADHD %>% select(-ID.code)
print(adult_ADHD)

# Rename the columns in the dataset
names(adult_ADHD) <- 1:18

# Verify the column names have been changed
View(adult_ADHD)

# descriptive analysis
summary(adult_ADHD)

describe(adult_ADHD) #this is an alternative for the summary function, it gives a better display

# Inspect the data
str(adult_ADHD)
head(adult_ADHD)

# Convert all items to numeric
adult_ADHD[] <- lapply(adult_ADHD, function(x) as.numeric(as.character(x)))

# Convert data to matrix format suitable for TAM
asrs_matrix <- as.matrix(adult_ADHD)

# Check the frequency of each response category for each item
apply(asrs_matrix, 2, table)

asrs_inattentive <- adult_ADHD[, c(1, 2, 3, 4, 5, 6, 7, 8, 9)] 
asrs_hyperactive_impulsive <- adult_ADHD[, c(10, 11, 12, 13, 14, 15, 16, 17, 18)]


# Fit Partial Credit rasch model
ADHD_rasch_model <- TAM::tam.mml(asrs_matrix, irtmodel = "PCM") #Tam automatically runs PCM when data is polytomous.
inattentive <- TAM::tam.mml(as.matrix(asrs_inattentive), irtmodel = "PCM")
hyper_impulse <- TAM::tam.mml(as.matrix(asrs_hyperactive_impulsive), irtmodel = "PCM")

# summary of the model
summary(ADHD_rasch_model)
summary(inattentive)
summary(hyper_impulse)
# Deltas
xsi <- ADHD_rasch_model$xsi

# get thresholds - Thurstone Thresholds get the cumulative values
Thresh <- as.data.frame(tam.threshold(ADHD_rasch_model))

#the code below exports the threshold value of the model data for predictive purposes 
write.csv(Thresh, here("Output", "ASRS_Threshold.csv"))

# transition to long data
thresh_pcm_long <- Thresh %>%
  mutate(item = rownames(.))%>%
  pivot_longer(cols = c(Cat1, Cat2, Cat3, Cat4))

head(thresh_pcm_long)

ggplot(data = thresh_pcm_long, aes(x=item, y=value, colour = name)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90))

# Delta-tau parameters
delta_tau <- ADHD_rasch_model$item_irt

View(ADHD_rasch_model$item) #PCM2 type parameteris

delta_tau <- delta_tau %>%
  mutate(HLS_cat1 = beta + tau.Cat1,
         HLS_cat2 = beta + tau.Cat2,
         HLS_cat3 = beta + tau.Cat3)

delta_tau

WLE.ability.poly <- tam.wle(ADHD_rasch_model)
WLE.ability.poly <- tam.wle(inattentive)
WLE.ability.poly <- tam.wle(hyper_impulse)

View(WLE.ability.poly)
#the reliability value for ASRS is higher than the AQ data, 0.865 value is a
#good while the reliabilty value for AQ is acceptable
person.ability.poly <- WLE.ability.poly$theta
head(person.ability.poly)

#the code below exports the theta column for predictive purposes 
write.csv(WLE.ability.poly, here("output", "ASRS_thetas.csv"))

#extracting the person score
asrs_scores <- WLE.ability.poly$PersonScores
head(asrs_scores)

#the code below exports the theta column for predictive purposes 
write.csv(asrs_scores, here("output", "asrs_scores.csv"))

#infit and outfit of each item
Fit.poly <- tam.fit(ADHD_rasch_model)

View(Fit.poly$itemfit)

#item characteristics curve for each item 
tthresh.poly <- tam.threshold(ADHD_rasch_model)
plot(ADHD_rasch_model, type = "items")

#polytomous Wright Map
wrightMap(person.ability.poly, tthresh.poly)
