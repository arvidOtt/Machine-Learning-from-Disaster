#Load dataset
titanic <- read.csv("./data/train.csv")
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

lastNames <- data.frame(t(matrix(
  unlist(strsplit(as.vector(titanic$Name), split = ",")), 
  ncol = length(titanic$Name), nrow = 2)))
titanic$lastname <- lastNames$X1
#Count occurences of lastname
occurences <- table(unlist(titanic$lastname))
titanic$lastnameCount <- occurences[titanic$lastname]

#Analyse missing age values
hist(titanic$Age)
titanic$AgeExists <- ifelse(is.na(titanic$Age),0,1)
table(titanic$AgeExist, titanic$Survived)
hist(titanic$Age)

#Fill missing Ages with mean age
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)

