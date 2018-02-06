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
titanic$Name <- as.character(titanic$Name)
titanic$FamilyName <- case_when(
  titanic$Title == "Mrs." ~ gsub("\\(.*", "", titanic$Name),
  titanic$Title == "Mr." ~ titanic$Name,
  TRUE ~ ""
)
titanic$FamilyName <- gsub("Mrs.","Mr.", titanic$FamilyName)
titanic$FamilyName <- factor(gsub(" ", "", titanic$FamilyName, fixed = TRUE))
occurences <- table(unlist(titanic$FamilyName))
titanic$FamilyNameCount <- occurences[titanic$FamilyName]
rm(occurences)
titanic$TravelsWithPartner <- ifelse(titanic$FamilyNameCount == 2,1,0)
titanic$TravelsWithPartner <- factor(titanic$TravelsWithPartner)
drops <- c("FamilyName", "FamilyNameCount")
titanic <- titanic[, !(names(titanic) %in% drops)]
rm(drops)

#Fill missing Ages with mean age
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)

#Missing Factor Level
levels(titanic$Embarked)[1] = "missing"

#Extract additional info from cabin
titanic$CabinLetter <- substr(titanic$Cabin,1,1)
titanic$CabinLetter <- factor(titanic$CabinLetter)
levels(titanic$CabinLetter)[1] = "missing"
levels(titanic$CabinLetter)

titanic$CabinNumber <- gsub("A|B|C|D|E|F|G|T","", titanic$Cabin)
titanic$CabinNumber <- as.numeric(titanic$CabinNumber)

#Extract additional info from tickets
titanic$TicketClass <- gsub(" |1|2|3|4|5|6|7|8|9|0|\\.|/","", titanic$Ticket)
titanic$TicketClass <- factor(titanic$TicketClass)
levels(titanic$TicketClass)[1] = "missing"

#Prepare for model
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)
drops <- c("Ticket", "Name","Cabin", "CabineNumber")
titanic <- titanic[, !(names(titanic) %in% drops)]
rm(drops)

#Train Decision Tree
library(C50)
treeModel <- C5.0(titanic[, -c(1:2)], titanic$Survived, trials = 14, control = C5.0Control(noGlobalPruning = FALSE))
summary(treeModel)


