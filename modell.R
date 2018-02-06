#Train Decision Tree
library(C50)
setwd("~/Kaggle/Titanic/Machine-Learning-from-Disaster")
train <- read.csv("./Data/train_p.csv")
train$Survived <- factor(train$Survived)
treeModel <- C5.0(train[, -c(1:3)], train$Survived, trials = 12, control = C5.0Control(noGlobalPruning = FALSE))
summary(treeModel)

#Evaluate Decision Tree
test <- read.csv("./Data/test_p.csv")
pred <- predict(treeModel, test[,-c(1:2)])

result <- data.frame(PassengerId=test$PassengerId, Survived=pred) 
write.csv(result[,-1],file = "./Data/prediction.csv")
            