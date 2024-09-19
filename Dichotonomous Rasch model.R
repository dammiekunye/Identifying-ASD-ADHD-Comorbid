# install.packages("TAM")
# install.packages("WrightMap")
# install.packages("Hmisc")
# install.packages("formattable")
#install.packages("ggplot2")
library("TAM") #for dichotomous Rasch analysis
library("WrightMap") # for plotting the variable map
library("Hmisc") # for descriptive data analysis
library("formattable") # for format number as percentage
library("readr") #load the data
library("dplyr") #for data manipulation
library("scales") # controls the apperance of axis and legend labels
#install.packages("here")
library("here")

autism_spectrum <- read.csv("Answers-AQ.csv")

#Define item indices for autistic traits subscales
aq_social <- autism_spectrum[, c(1, 11, 13, 15, 22, 36, 44, 45, 47, 48)] 
aq_attention_switching <- autism_spectrum[, c(2, 4, 10, 16, 25, 32, 34,37,43,46)]
aq_attention_detail <- autism_spectrum[, c(5, 6, 9, 12, 19, 23, 28, 29, 30, 49)]
aq_communication <- autism_spectrum[, c(7, 17, 18, 26, 27, 31, 33, 35, 38, 39)]
aq_imagination <- autism_spectrum[, c(3, 8, 14, 20, 21, 24, 40, 41, 42, 50)]
# Rename the columns in the dataset
names(autism_spectrum) <- 1:50

# Verify the column names have been changed
View(autism_spectrum)

# descriptive analysis
summary(autism_spectrum)

describe(autism_spectrum) #this is an alternative for the summary function, it gives a better display

# running dichotomous rasch model
Di_rasch_model <- tam(autism_spectrum)
social_rasch_model <- tam(aq_social)
att_switching_rasch_model <- tam(aq_attention_switching)
att_detail_rasch_model <- tam(aq_attention_detail)
comm_rasch_model <- tam(aq_communication)
imag_rasch_model <- tam(aq_imagination)
# summary of the model
summary(Di_rasch_model)

#ITEM DIFFICULTY
# examine item difficulty parameter
difficulty <- Di_rasch_model$xsi
difficulty

#item difficulty summary
summary(difficulty)

#item characteristics curve for each item
plot(Di_rasch_model, items = 1:50)

sd(difficulty$xsi)

mean(difficulty$xsi)

#histogram plot for item difficulty
hist(difficulty$xsi, main = "Histogram of Item Difficulty Estimates for the Transitive Reasoning Data",
     xlab = "Item Difficulty Estimates in Logits") 

#scatter plot for item difficulty
plot(difficulty$xsi, main="Scatter Plot of Item Difficulties", xlab="Item Number", ylab = "Difficulty in Logits", pch=9)
axis(side=1, at = c(1:50))

# a better histogram plot 
ggplot(difficulty, aes(x = xsi)) +
  geom_histogram(bins=50) +
  ggtitle("Distribution of Item Difficulties")

#item difficulties against standard error
ggplot(difficulty, aes(x = xsi, y=se.xsi)) + geom_point() +
  ggtitle("Item difficulties and their standard error") +
  xlab("Estimated Item Difficulties") +
  ylab("Estimated Item Standard Errors")

#ITEM FIT
item_fit <- tam.fit(Di_rasch_model)

summary(item_fit)

str(item_fit)

itemfit_table = item_fit$itemfit

View(itemfit_table)
sd(itemfit_table$Outfit)
mean(itemfit_table$Outfit)
sd(itemfit_table$Outfit_t)
mean(itemfit_table$Outfit_t)
sd(itemfit_table$Outfit_p)
mean(itemfit_table$Outfit_p)
sd(itemfit_table$Infit)
mean(itemfit_table$Infit)
sd(itemfit_table$Infit_t)
mean(itemfit_table$Infit_t)
sd(itemfit_table$Infit_p)
mean(itemfit_table$Infit_p)
write.table(itemfit_table, file= "Exporteditemfit.csv", row.names=F, sep=',')

#visualizing the item fit
infit <- item_fit$itemfit$Infit

upper_bound <- rep(x = 1.33, times =50) # this repeats 1.33 fifty times
lower_bound <- rep(x = .75, times = 50) 

# running fitgraph
fitgraph(fitEst = infit, fitLB = lower_bound, fitUB = upper_bound, itemLabels = names(autism_spectrum))

# outfit plot
outfit <- item_fit$itemfit$Outfit


fitgraph(fitEst = outfit, fitLB = lower_bound, fitUB = upper_bound, itemLabels = names(autism_spectrum))

# put the fit data in a dataframe
fit_stats <- item_fit$itemfit

fit_stats %>%
  ggplot(aes(x=parameter, y = infit)) + 
  geom_point() + 
  geom_hline(yintercept = 1.2, linetype="dashed", color="red") +
  geom_hline(yintercept = .8, linetype="dashed", color="blue") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  ggtitle("Item Fit Statistics for Autism Spectrum Data")

# Extract item parameters (item difficulties in this case)
item_prop <- Di_rasch_model$item
item_prop

apply(autism_spectrum[1:50], 2, sum)#sum total of answers 1 for each item 1-50

# creating a column in the item_prop object that has the total number of endorsements for each item
item_prop <- mutate(item_prop, total_endorsed =N*M)

#checking the correlation between the total number of endorsements per item 
#and the estimated item difficulty
cor(item_prop$xsi.item, item_prop$total_endorsed)

#visualizing the correlation between total number of endorsements per item and the estimated item difficulty
ggplot(item_prop, aes(x=total_endorsed, y=xsi.item)) + 
  geom_point() +
  ylab("Estimated Item Difficulties (logits)") +
  xlab("Total Number of Endorsements for an item") +
  ggtitle("Relationship between estimated item difficulty and total endorsements")

#calculate person locations that correspond to our model
person_abil <- as.data.frame(tam.wle(Di_rasch_model))
View(person_abil)


person_abil_social <- as.data.frame(tam.wle(social_rasch_model))
View(person_abil_social)
person_abil_att_switch <- as.data.frame(tam.wle(att_switching_rasch_model))
person_abil_att_detail <- as.data.frame(tam.wle(att_detail_rasch_model))
person_abil_comm <- as.data.frame(tam.wle(comm_rasch_model))
person_abil_imag <- as.data.frame(tam.wle(imag_rasch_model))
#descriptive statistics of person locations
summary(person_abil)

PersonAbility <- person_abil$theta

TotalPersonScore <- person_abil$PersonScores

#the code below exports the theta column for predictive purposes 
write.csv(person_abil, here("Output", "AQ_thetas.csv"))

hist(PersonAbility, main = "Histogram of Person Achievement Estimates \nfor the Transitive Reasoning Data",
     xlab = "Person Achievement Estimates in Logits(Person Ability)") 

mean(PersonAbility)

sd(PersonAbility)

Person_fit <- tam.personfit(Di_rasch_model) 

summary(Person_fit)

#To visualize the relationship between item difficulty and person ability distributions, call the WrightMap
# Plot the Wright Map 
IRT.WrightMap(Di_rasch_model, show.thr.lab=FALSE, main.title = "Transitive Reasoning Wright Map")

#Establishing a clinical threshold for high AQ-50 as 
threshold_aq <- quantile(PersonAbility, 0.95)
View(threshold_aq)
