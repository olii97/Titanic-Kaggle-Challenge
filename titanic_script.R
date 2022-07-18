# Read data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Install "rpart"
#install.packages("rpart")
library(rpart)

# Build 1st tree
tree1 <- rpart(Survived ~ Sex + Age, data = train, method = "class")
plot(tree1)
text(tree1)

# 1st prediction, result: 0.75598
prediction1 <- predict(tree1, newdata = test, type = "class")
prediction_DF_1 <- data.frame(PassengerID = test$PassengerId, Survived = prediction1)
write.csv(prediction_DF_1, file="first_tree.csv", row.names = F)

# 2nd prediction, result: 0.78469
tree2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
prediction2 <- predict(tree2, newdata = test, type = "class")
prediction_DF_2 <- data.frame(PassengerID = test$PassengerId, Survived = prediction2)
write.csv(prediction_DF_2, file="second_tree.csv", row.names = F)

# 3rd precition, by dig out more information from name: whether includes "Master"
# Result: 0.79904
# Vector store if passenger is a Master 
isMaster <- grepl(pattern = "Master", train$Name, useBytes = T)
train_update3 <- data.frame(train, isMaster = isMaster)

# Build decision tree 3, adding doctor status into data set
tree3 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + isMaster, data = train_update3, method = "class")
isMS_test <- grepl(pattern = "Master", test$Name, useBytes = T)
test_MS <- data.frame(test, isMaster = isMS_test)
prediction3 <- predict(tree3, newdata = test_MS, type = "class")
prediction_DF_3 <- data.frame(PassengerID = test_Dr$PassengerId, Survived = prediction3)
write.csv(prediction_DF_3, file="thrid_tree.csv", row.names = F)


# 4th precition, by dig out more information from name: whether includes "Master" & "Dr."
# 
# Vector store if passenger is a Master & Dr. 
isMD <- grepl(pattern = "Master", train$Name, useBytes = T) | grepl(pattern = "Dr. ", train$Name, useBytes = T)
train_update4 <- data.frame(train, isMD = isMD)

# Build decision tree 4, adding doctor/master status into data set
tree4 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + isMD, data = train_update4, method = "class")
isMD_test <- grepl(pattern = "Master", test$Name, useBytes = T) | grepl(pattern = "Dr. ", test$Name, useBytes = T)
test_MD <- data.frame(test, isMD = isMD_test)
prediction4 <- predict(tree4, newdata = test_MD, type = "class")
prediction_DF_4 <- data.frame(PassengerID = test_MD$PassengerId, Survived = prediction4)
write.csv(prediction_DF_4, file="forth_tree.csv", row.names = F)


# 5 prediction, add status of cabin class
# Result: 0.79904, the same, it seems cabin doesn't provide new information
isMD <- grepl(pattern = "Master", train$Name, useBytes = T) | grepl(pattern = "Dr. ", train$Name, useBytes = T)
cabinStat <- as.numeric(grepl(pattern = "A", train$Cabin, useBytes = T))  +
            2*as.numeric(grepl(pattern = "B", train$Cabin, useBytes = T)) +
            3*as.numeric(grepl(pattern = "C", train$Cabin, useBytes = T)) +
            4*as.numeric(grepl(pattern = "D", train$Cabin, useBytes = T)) +
            5*as.numeric(grepl(pattern = "E", train$Cabin, useBytes = T)) +
            6*as.numeric(grepl(pattern = "F", train$Cabin, useBytes = T)) +
            7*as.numeric(grepl(pattern = "G", train$Cabin, useBytes = T)) +
            8*as.numeric(grepl(pattern = "T", train$Cabin, useBytes = T))
train_update5 <- data.frame(train, isMD = isMD, cabinStat = cabinStat)

# Build decision tree 5, adding doctor/master status into data set
tree5 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + isMD + cabinStat, data = train_update5, method = "class")
isMD_test <- grepl(pattern = "Master", test$Name, useBytes = T) | grepl(pattern = "Dr. ", test$Name, useBytes = T)
cabinStat_test <- as.numeric(grepl(pattern = "A", test$Cabin, useBytes = T))  +
  2*as.numeric(grepl(pattern = "B", test$Cabin, useBytes = T)) +
  3*as.numeric(grepl(pattern = "C", test$Cabin, useBytes = T)) +
  4*as.numeric(grepl(pattern = "D", test$Cabin, useBytes = T)) +
  5*as.numeric(grepl(pattern = "E", test$Cabin, useBytes = T)) +
  6*as.numeric(grepl(pattern = "F", test$Cabin, useBytes = T)) +
  7*as.numeric(grepl(pattern = "G", test$Cabin, useBytes = T)) +
  8*as.numeric(grepl(pattern = "T", test$Cabin, useBytes = T))


test_5 <- data.frame(test, isMD = isMD_test, cabinStat = cabinStat_test)
prediction5 <- predict(tree5, newdata = test_5, type = "class")
prediction_DF_5 <- data.frame(PassengerID = test_5$PassengerId, Survived = prediction5)
write.csv(prediction_DF_5, file="fifth_tree.csv", row.names = F)



# 6th precition, by dig out more information from name: whether includes "Master" & "Dr."
# And title: whether there's Miss or Mr
# Result: 0.80383
# Vector store if passenger is a Master & Dr. 
isMD <- grepl(pattern = "Master", train$Name, useBytes = T) | grepl(pattern = "Dr. ", train$Name, useBytes = T)
isMiss <- grepl(pattern = "Miss", train$Name, useBytes = T)
isMr <- grepl(pattern = "Mr. ", train$Name, useBytes = T)

train_update6 <- data.frame(train, isMD = isMD, isMiss = isMiss, isMr = isMr)

# Build decision tree 4, adding doctor/master status into data set
tree6 <- rpart(Survived ~ Pclass + Sex + Age + Fare + isMD + isMiss + isMr, data = train_update6, method = "class")
isMD_test <- grepl(pattern = "Master", test$Name, useBytes = T) | grepl(pattern = "Dr. ", test$Name, useBytes = T)
isMiss_test <- grepl(pattern = "Miss", test$Name, useBytes = T)
isMr_test <- grepl(pattern = "Mr. ", test$Name, useBytes = T)

test_6 <- data.frame(test, isMD = isMD_test, isMiss = isMiss_test, isMr = isMr_test)
prediction6 <- predict(tree6, newdata = test_6, type = "class")
prediction_DF_6 <- data.frame(PassengerID = test_MD$PassengerId, Survived = prediction6)
write.csv(prediction_DF_6, file="sixth_tree2.csv", row.names = F)
