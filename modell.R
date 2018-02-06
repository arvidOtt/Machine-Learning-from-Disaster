#Train Decision Tree
library(C50)
setwd("~/Kaggle/Titanic/Machine-Learning-from-Disaster")
train <- read.csv("./Data/train_p.csv")
train$Pclass <- factor(train$Pclass)
train$TravelsWithPartner <- factor(train$TravelsWithPartner)
train$Survived <- factor(train$Survived)
treeModel <- C5.0(train[, -c(1:3)], train$Survived, trials = 10)
summary(treeModel)

#Evaluate Decision Tree
test <- read.csv("./Data/test_p.csv")
pred <- predict(treeModel, test[,-c(1:2)])

result <- data.frame(PassengerId=test$PassengerId, Survived=pred)
write.csv(result,file = "./Data/prediction.csv", row.names = FALSE)

            