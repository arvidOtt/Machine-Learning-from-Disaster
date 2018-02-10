#Load Data
setwd("~/Kaggle/Titanic/Machine-Learning-from-Disaster")
train <- read.csv("./Data/train_p.csv",header = TRUE, fileEncoding = "UTF-8")

#Prepare for model
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)

drops <- c("Ticket", "Name","Cabin", "Embarked")
train <- train[, !(names(train) %in% drops)]
rm(drops)

#Decision Tree
library(C50)
treeModel <- C5.0(train[, -c(1:3)], train$Survived, control = C5.0Control(minCases = 40))
summary(treeModel)

#Load test data
test <- read.csv("./Data/test_p.csv")

#Prepare data
test$Pclass <- factor(test$Pclass)
drops <- c("Ticket", "Name","Cabin", "Embarked")
test <- test[, !(names(test) %in% drops)]
rm(drops)

#Evaluate Model
pred <- predict(treeModel, test[,-c(1:2)])
result <- data.frame(PassengerId=test$PassengerId, Survived=pred)
write.csv(result,file = "./Data/prediction.csv", row.names = FALSE)

            