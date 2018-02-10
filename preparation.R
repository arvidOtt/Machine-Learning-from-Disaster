#Load dataset
setwd("~/Kaggle/Titanic/Machine-Learning-from-Disaster")
#titanic <- read.csv("./Data/train.csv")
titanic <- read.csv("./Data/test.csv")

#Extract Title
library(dplyr)
titanic$Title <- case_when(
  grepl("Mrs.",titanic$Name) ~ "Mrs",
  grepl("Mr.",titanic$Name) ~ "Mr",
  grepl("Miss.",titanic$Name) ~ "Miss",
  grepl("Master.",titanic$Name) ~ "Master",
  grepl("Dr.",titanic$Name) ~ "Dr",
  grepl("Rev.",titanic$Name) ~ "Rev",
  TRUE ~ "Other"
)

#Extract familyname to match couples
#Pobably bad idea because wife neednt be in the same dataset
#titanic$Name <- as.character(titanic$Name)
#titanic$FamilyName <- case_when(
#  titanic$Title == "Mrs" ~ gsub("\\(.*", "", titanic$Name),
#  titanic$Title == "Mr" ~ titanic$Name,
#  TRUE ~ ""
#)
#titanic$FamilyName <- gsub("Mrs.","Mr.", titanic$FamilyName)
#titanic$FamilyName <- factor(gsub(" ", "", titanic$FamilyName, fixed = TRUE))
#occurences <- table(unlist(titanic$FamilyName))
#titanic$FamilyNameCount <- occurences[titanic$FamilyName]
#rm(occurences)
#titanic$TravelsWithPartner <- ifelse(titanic$FamilyNameCount == 2,1,0)
#titanic$TravelsWithPartner <- factor(titanic$TravelsWithPartner)
#drops <- c("FamilyName", "FamilyNameCount")
#titanic <- titanic[, !(names(titanic) %in% drops)]
#rm(drops)

#Fill missing Ages with mean age
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)

#Extract additional info from cabin
#titanic$CabinLetter <- substr(titanic$Cabin,1,1)
#titanic$CabinLetter <- factor(titanic$CabinLetter)
#levels(titanic$CabinLetter)[1] = "missing"
#levels(titanic$CabinLetter)

#titanic$CabinNumber <- gsub("A|B|C|D|E|F|G|T","", titanic$Cabin)
#titanic$CabinNumber <- as.numeric(titanic$CabinNumber)

#Extract additional info from tickets
titanic$TicketClass <- gsub('\\D+','', titanic$Ticket)
titanic$TicketClass <- substr(titanic$TicketClass,1,1)
titanic$TicketClass <- gsub('4|5|6|7|8|9','4', titanic$TicketClass)

#Missing Factor Levels
levels(titanic$Cabin)[1] = "missing"
levels(titanic$Embarked)[1] = "missing"

#Save data
#write.csv(titanic, file = "./Data/train_p.csv")
write.csv(titanic, file = "./Data/test_p.csv")



