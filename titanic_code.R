#Load dataset
setwd("~/Kaggle/Titanic/Machine-Learning-from-Disaster")
titanic <- read.csv("./Data/train.csv")
summary(titanic)

#Extract Title
library(dplyr)
titanic$Title <- case_when(
  grepl("Mrs.",titanic$Name) ~ "Mrs.",
  grepl("Mr.",titanic$Name) ~ "Mr.",
  grepl("Miss.",titanic$Name) ~ "Miss.",
  grepl("Master.",titanic$Name) ~ "Master.",
  grepl("Dr.",titanic$Name) ~ "Dr.",
  grepl("Rev.",titanic$Name) ~ "Rev.",
  TRUE ~ "Other"
)
titanic$Title <- factor(titanic$Title)

#Extract familyname to match couples
titanic$FamilyName <- case_when(
  titanic$Title == "Mrs." ~ gsub("\\(.*", "", titanic$Name),
  titanic$Title == "Mr." ~ titanic$Name,
  TRUE ~ ""
)
titanic$FamilyName <- gsub("Mrs.","Mr.", titanic$FamilyName)
titanic$FamilyName <- factor(gsub(" ", "", titanic$FamilyName, fixed = TRUE))
occurences <- table(unlist(titanic$FamilyName))
summary(occurences)
titanic$FamilyNameCount <- occurences[titanic$FamilyName]
rm(occurences)
table(titanic$FamilyNameCount)
titanic$TravelsWithPartner <- ifelse(titanic$FamilyNameCount == 2,1,0)
titanic$TravelsWithPartner <- factor(titanic$TravelsWithPartner)
drops <- c("FamilyName", "FamilyNameCount")
titanic <- titanic[, !(names(titanic) %in% drops)]
rm(drops)

#Fill missing Ages with mean age
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)

#Analyse Cabins

#Train Decision Tree
library(C50)
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)
levels(titanic$Embarked)[1] = "missing"
drops <- c("Ticket", "Cabin","Name")
titanic <- titanic[, !(names(titanic) %in% drops)]
rm(drops)
summary(titanic)
treeModel <- C5.0(titanic[, -2], titanic$Survived)
summary(treeModel)
