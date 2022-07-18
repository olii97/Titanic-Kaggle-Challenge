# Loading packages
library("tidymodels")
library("themis")
library("knitr")
library("ranger")
library("doParallel")
library("vip")
library("skimr")
library("corrplot")
library("ggridges")


# Read data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

glimpse(train)

# Classification issue
train %>% count(Survived) %>% 
  mutate(prop = n / sum(n))

# Check all the variables
skim(train)

# Corrplot without age
corrplot(cor(train %>% select(PassengerId, Survived, Pclass,  SibSp, Parch, Fare)))


