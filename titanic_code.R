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

#Extract lastname
titanic$Name <- as.character(titanic$Name)
LastNames <- data.frame(t(matrix(
  unlist(strsplit(as.vector(titanic$Name), split = ",")), 
  ncol = length(titanic$Name), nrow = 2)))
titanic$Lastname <- LastNames$X1
#Count lastname
occurences <- table(unlist(titanic$Lastname))
titanic$LastnameCount <- occurences[titanic$Lastname]
drops <- c("Lastname")
titanic <- titanic[, !(names(titanic) %in% drops)]
rm(occurences, LastNames, drops)

#Extract familyname
titanic$FamilyName <- case_when(
  titanic$Title == "Mrs." ~ gsub("\\(.*", "", titanic$Name),
  titanic$Title == "Mr." ~ titanic$Name,
  TRUE ~ ""
)
titanic$FamilyName <- gsub("Mrs.","Mr.", titanic$FamilyName)
titanic$FamilyName <- factor(gsub(" ", "", titanic$FamilyName, fixed = TRUE))
#Count familyname
occurences <- table(unlist(titanic$FamilyName))
summary(occurences)
titanic$FamilyNameCount <- occurences[titanic$FamilyName]
rm(occurences)
table(titanic$FamilyNameCount)
titanic$TravelsWithPartner <- ifelse(titanic$FamilyNameCount == 2,1,0)
drops <- c("FamilyName", "FamilyNameCount")
titanic <- titanic[, !(names(titanic) %in% drops)]
rm(drops)

#Analyse missing age values
hist(titanic$Age)
titanic$AgeExists <- ifelse(is.na(titanic$Age),0,1)
table(titanic$AgeExist, titanic$Survived)
hist(titanic$Age)

#Fill missing Ages with mean age
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)

