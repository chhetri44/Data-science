# Who survived the Sinking of The Titanic?
# Version: 1.0
# Author: Chhetri


# Load the required packages

# A system for 'declaratively' creating graphics, 
# based on "The Grammar of Graphics". 
# You provide the data, 
# tell 'ggplot2' how to map variables to aesthetics, 
# what graphical primitives to use, 
# and it takes care of the details.
library('ggplot2')  

# This package contains extra themes, 
# scales, and geoms, and functions 
# for and related to ggplot2.
library('ggthemes') 

# visualization - Generic plot scaling methods
library('scales') 

# dplyr provides a flexible grammar of data manipulation. 
# It's the next iteration of plyr, 
# focused on tools for working with data frames 
# (hence the d in the name).
library('dplyr')

# The mice package implements a method to deal with missing data.
library('mice')  

# randomForest implements Breiman's random forest algorithm 
# (based on Breiman and Cutler's original Fortran code) 
# for classification and regression. 
# It can also be used in unsupervised mode 
# for assessing proximities among data points.
library('randomForest') 

# Package to predict race/ethnicity on the basis of surnames 
library('wru')

# This function reshapes a data frame between ‘wide’ format 
# with repeated measurements in separate columns of the same record 
# and ‘long’ format with the repeated measurements in separate records.
library(reshape2)

# Setting the working directory
getwd()
setwd("/home")

# Importing Competition datasets
train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

# bind the datasets together
test_train_combined  <- bind_rows(train, test) 
describe(test_train_combined)
# check data
str(test_train_combined)

head(test_train_combined)

# Step 1 
# Feature engineering
# Creation of new features and mising value treatment
#


# Create field Title from passenger names
test_train_combined$Title <- gsub('(.*, )|(\\..*)', '', test_train_combined$Name)

# Title-Sex crosstabulated counts
table(test_train_combined$Sex, test_train_combined$Title)


# Titles with low counts can be combined to "Others" level
other <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Reassign mlle, ms, and mme as they seem to be errors in consistency
test_train_combined$Title[test_train_combined$Title == 'Mlle']        <- 'Miss' 
test_train_combined$Title[test_train_combined$Title == 'Ms']          <- 'Miss'
test_train_combined$Title[test_train_combined$Title == 'Mme']         <- 'Mrs' 
test_train_combined$Title[test_train_combined$Title %in% other]  <- 'Others'

# Title-sex cros tabulated counts
table(test_train_combined$Sex, test_train_combined$Title)

describe(test_train_combined)

# Extract surname from passenger name
test_train_combined$Surname <- sapply(test_train_combined$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
test_train_combined$Fsize <- test_train_combined$SibSp + test_train_combined$Parch + 1

# Create a family variable 
test_train_combined$Family <- paste(test_train_combined$Surname, test_train_combined$Fsize, sep='_')

# Use ggplot2 to visualize the relationship between family size & survival
# Note only the first 891 rows in the train data is being analyzed below
ggplot(test_train_combined[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


# Discretize family size
test_train_combined$FsizeD[test_train_combined$Fsize == 1] <- 'singleton'
test_train_combined$FsizeD[test_train_combined$Fsize < 5 & test_train_combined$Fsize > 1] <- 'small'
test_train_combined$FsizeD[test_train_combined$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(test_train_combined$FsizeD, test_train_combined$Survived), main='Family Size by Survival', shade=TRUE)


# This variable appears to have a lot of missing values
test_train_combined$Cabin[1:28]

# The first character is the deck. For example:
strsplit(test_train_combined$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
test_train_combined$Deck<-factor(sapply(test_train_combined$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

describe(test_train_combined)

# Passengers 62 and 830 are missing Embarked flag
test_train_combined[c(62, 830),]

# Create another dataset without Passenger ID 62 & 830
embark_fare <- test_train_combined %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# $80 is for 1st class based on median from last plot, they may have embarked from 'C'
test_train_combined$Embarked[c(62, 830)] <- 'C'

# Show row 1044
test_train_combined[1044, ]

ggplot(test_train_combined[test_train_combined$Pclass == '3' & test_train_combined$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# Replace missing fare value with median fare for class/embarkment
test_train_combined$Fare[1044] <- median(test_train_combined[test_train_combined$Pclass == '3' & test_train_combined$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Show number of missing Age values
sum(is.na(test_train_combined$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

test_train_combined[factor_vars] <- lapply(test_train_combined[factor_vars], function(x) as.factor(x))

describe(test_train_combined)

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(test_train_combined[, !names(test_train_combined) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(test_train_combined$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
test_train_combined$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(test_train_combined$Age))

# First we'll look at the relationship between age & survival
ggplot(test_train_combined[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()


# Create the column child, and indicate whether child or adult
test_train_combined$Child[test_train_combined$Age < 18] <- 'Child'
test_train_combined$Child[test_train_combined$Age >= 18] <- 'Adult'

# Show counts
table(test_train_combined$Child, test_train_combined$Survived)

# Adding Mother variable
test_train_combined$Mother <- 'Not Mother'
test_train_combined$Mother[test_train_combined$Sex == 'female' & test_train_combined$Parch > 0 & test_train_combined$Age > 18 & test_train_combined$Title != 'Miss'] <- 'Mother'

# Show counts
table(test_train_combined$Mother, test_train_combined$Survived)

# Finish by factorizing our two new factor variables
test_train_combined$Child  <- factor(test_train_combined$Child)
test_train_combined$Mother <- factor(test_train_combined$Mother)

md.pattern(test_train_combined)

describe(test_train_combined)

# Predicting races on basis of surnames

# Create Data frame for prediction
race_predict_df <- data.frame(test_train_combined$PassengerId,test_train_combined$Surname)

# Renaming Surname column
colnames(race_predict_df)[2] <- "surname"

# Predicting the Race
predicted_scores <- predict_race(race_predict_df, surname.only = TRUE)

# Creating flag for max score 
predicted_scores$max_flag <- apply(X=predicted_scores[3:7], MARGIN=1, FUN=max)

#Getting the most likely ethnicity on basis of prediction value 
race_filtered<-filter(melt(predicted_scores, id = c("test_train_combined.PassengerId","surname", "max_flag")), value-max_flag ==0)

# For final filtering on column names
keeps <- c("test_train_combined.PassengerId", "variable")
race_final <- race_filtered[keeps]

# Rename PassengerId & Variable
colnames(race_final)[1] <- "PassengerId"
colnames(race_final)[2] <- "race"

# Merge with test_train_combined
test_train_combined <- merge(x = test_train_combined, y = race_final, by = "PassengerId", all.x = TRUE)
test_train_combined <- test_train_combined[order(test_train_combined$PassengerId),] 
rownames(test_train_combined) <- 1:nrow(test_train_combined)

# Any relation with survival
table(test_train_combined$race,test_train_combined$Survived)

# Factorize the race
test_train_combined$race  <- factor(test_train_combined$race)

# Finding kinds of tickets
x<-unique(data.frame(substr(test_train_combined$Ticket,1,1)))

# Considering tickets with alphanumeric as special
test_train_combined$ticketfrom[substr(test_train_combined$Ticket,1,1) == 'A' |
                                 substr(test_train_combined$Ticket,1,1) == 'C' |
                                 substr(test_train_combined$Ticket,1,1) == 'F'|
                                 substr(test_train_combined$Ticket,1,1) == 'L'|
                                 substr(test_train_combined$Ticket,1,1) == 'P'|
                                 substr(test_train_combined$Ticket,1,1) == 'S'|
                                 substr(test_train_combined$Ticket,1,1) == 'W'] <- 'Special'

test_train_combined$ticketfrom[!(substr(test_train_combined$Ticket,1,1) == 'A' |
                                   substr(test_train_combined$Ticket,1,1) == 'C' |
                                   substr(test_train_combined$Ticket,1,1) == 'F'|
                                   substr(test_train_combined$Ticket,1,1) == 'L'|
                                   substr(test_train_combined$Ticket,1,1) == 'P'|
                                   substr(test_train_combined$Ticket,1,1) == 'S'|
                                   substr(test_train_combined$Ticket,1,1) == 'W')] <- 'Not Special'

# Factorize the column
test_train_combined$ticketfrom  <- factor(test_train_combined$ticketfrom)

table(test_train_combined$Survived,test_train_combined$ticketfrom)

# Checking the table
describe(test_train_combined)


# Getting the number of people by family onboard the ship
actual_passenger<-data.frame(table(test_train_combined$Family))
colnames(actual_passenger)[1] <- "Family"

# Getting supposed to trave number of people by family
supposed_passenger <- data.frame(aggregate(test_train_combined$Fsize, list(Family = test_train_combined$Family), mean))

# Combine the two datasets
actual_supposed_family<- data.frame(merge(x = actual_passenger,y = supposed_passenger,by = "Family", all.x = T))

# In case of mismatch assign No to feature FullFam
actual_supposed_family$fullfam <- "Yes"
actual_supposed_family$fullfam[actual_supposed_family$Freq != actual_supposed_family$x ] <-"No" 

# Full family feature
test_train_combined <- merge(x = test_train_combined, y = actual_supposed_family[c(1, 4)], by = "Family", all.x = T)

# Check data
describe(test_train_combined)

# See effect
table(test_train_combined$Survived,test_train_combined$fullfam)

# Factorize the column
test_train_combined$fullfam  <- factor(test_train_combined$fullfam)

# Fiz row indexes
test_train_combined <- test_train_combined[order(test_train_combined$PassengerId),] 
rownames(test_train_combined) <- 1:nrow(test_train_combined)

# Step 2
# Survival Prediction
#

# Split the data back into a train set and a test set
train <- test_train_combined[1:891,]
test <- test_train_combined[892:1309,]

# Set a random seed
#set.seed(754)

set.seed(29152)# - w/o fullfam, ticketfrom and any parameter

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother + race,
                         data = train, corr.bias=F, ntree=635, maxnodes=142)
# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

mean(rf_model$err.rate)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_model_Solution_V2.csv', row.names = F)

