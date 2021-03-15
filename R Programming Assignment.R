#####################   Nukavarapu Lalita Aditya Patel _ MBA(BA.AI.ML) _ 2002020 _ R Programming Assignment _ 14 - 03 - 2021 ################################

# Importing of required libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(brio)
library(curl)
library(readxl)
library(readtext)
library(corrplot)
library(MLmetrics)
library(rvest)
library(desc)
library(gapminder)
library(ggcorrplot)

# Reading the GitHub file

url <- "https://github.com/SavioSal/datasets/raw/master/Bank%20Churn_Modelling.csv"

bank_churn_modelling <- read_csv(url) # reads the url file 

view(bank_churn_modelling) # using this command gives us an overview towards what is present in the downloaded dataset

str(bank_churn_modelling)

names(bank_churn_modelling) # this command will help us in knowing what the names of columns are present in the dataset. 

head(bank_churn_modelling) # using this command will display the data that is present in the dataset

# Data Cleaning 

bank_churn_modelling <- bank_churn_modelling %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname) %>% #removes the unwanted columns 
  mutate(Geography = as.factor(Geography),
         Gender = as.factor(Gender),
         HasCrCard = as.factor(HasCrCard),
         IsActiveMember = as.factor(IsActiveMember),
         Exited = as.factor(Exited),
         Tenure = as.factor(Tenure),
         NumOfProducts = as.factor(NumOfProducts))

# Checking for Missing and Null Values 

sapply(bank_churn_modelling, function(x) sum(is.na(x)))

# Data Overview

summary(bank_churn_modelling)

#### 1. Develop 5 different visuals using GGPLOT with descriptions of the insights they convey

### Categorical Distribution of the variables from the given Dataset  

bank_churn_modelling %>%
  dplyr::select(-Exited) %>% 
  keep(is.factor) %>%
  gather() %>%
  group_by(key, value) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_bar(mapping=aes(x = value, y = n, fill=key), color="black", stat='identity') + 
  coord_flip() +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

### Continuous Distribution of the Variables from the given dataset

bank_churn_modelling %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() + geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") + theme_minimal() + theme(legend.position = 'none')

### Who are the churned customers and non-churned customers

ggplot(bank_churn_modelling, aes(Exited, fill = Exited)) +
  geom_bar() +
  theme(legend.position = 'none')

table(bank_churn_modelling$Exited)

### Finding of the Correlation Matrix for Continuous Variables

numericVarName <- names(which(sapply(bank_churn_modelling, is.numeric)))
corr <- cor(bank_churn_modelling[,numericVarName], use = 'pairwise.complete.obs')
ggcorrplot(corr, lab = TRUE)

### which age group category of customers fall under more churn

ggplot(bank_churn_modelling, aes(Exited, Age, fill = Exited)) +
  geom_boxplot() + 
  theme_minimal() +
  theme(legend.position = 'none')

### Account balance comparison between churned customers and non - churned customers

ggplot(bank_churn_modelling, aes(x = Exited, y = Balance, fill = Exited)) +
  geom_boxplot() + 
  theme_minimal() +
  theme(legend.position = 'none')

### Estimated Salary of churned and non - churned customers

ggplot(bank_churn_modelling, aes(x = Exited, y = EstimatedSalary, fill = Exited)) +
  geom_boxplot() + 
  theme_minimal() +
  theme(legend.position = 'none')


#### 2. Develop answers to the following. Use dplyr wherever necessary:

###  A.What is the average credit score of females and males in France?

bank_churn_modelling %>% filter(Geography == "France") %>%  group_by(Gender) %>% summarise(mean_crScore = round(mean(CreditScore),2))

###  B. What is the average credit score of people in the age brackets 20-30,31-40,41-50?

bank_churn_modelling %>% select(CreditScore, Age) %>% mutate(AgeCategory = case_when(Age >= 20 & Age <= 30 ~ '20 - 30', Age >= 31 & Age <= 40 ~ '31 - 40', Age >= 41 & Age <= 50 ~'41 - 50')) %>% 
  filter(AgeCategory == "20 - 30" | AgeCategory == '31 - 40' | AgeCategory == '41 - 50') %>% 
  group_by(AgeCategory) %>%  
  summarise(mean_crScore = round(mean(CreditScore),2))

###  C. What is the correlation between credit score and estimated salary?  

bank_churn_modelling %>% select(CreditScore, EstimatedSalary) %>% cor()

plot(bank_churn_modelling$CreditScore, bank_churn_modelling$EstimatedSalary, main="Correlation between Credit Score and Estimated Salary", xlab="Credit Score", ylab="Estimated Salary")

###  D. Develop a statistical model to explain and establish a mathematical relationship between credit score (dependent) and gender, age, estimate salary.

Stat_mod <- bank_churn_modelling %>% select(Gender, Age, EstimatedSalary, CreditScore)

Stat_mod$Gender <- as.numeric(factor(Stat_mod$Gender))

cor(Stat_mod)

corrplot(cor(Stat_mod), method = "number")

set.seed(123)

train = sample(1:nrow(Stat_mod), nrow(Stat_mod)*0.2)

stat_mod_train <- Stat_mod[train,]

stat_mod_test <- Stat_mod[-train,]

stat_mod_lm = lm(formula = CreditScore ~ Age + EstimatedSalary, data = bank_churn_modelling)

y_pred <-  predict(stat_mod_lm, stat_mod_test)

y_true <-  stat_mod_test$CreditScore

MSE(y_pred, y_true)

summary(stat_mod_lm)

