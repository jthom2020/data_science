#Titanic

#0:Load the data in RStudio
titanic_df <- read.csv("/Users/jonathonthompson/Documents/data_science/sb_datawrangling_2/titanic_original.csv")

str(titanic_df)
head(titanic_df)


#1: Port of embarkation
titanic_df$embarked <- gsub("^$", "S", titanic_df$embarked)
##titanic_df$embarked[grep("^$", titanic_df$embarked)] <- "S" 

#2: Age
##1
titanic_df$age[is.na(titanic_df$age)] <- mean(titanic_df$age, na.rm = TRUE)
#age <- mean(titanic_df$age, na.rm = TRUE)
#grep("^$", titanic_df$age)
#filter(titanic_df, is.na(age))
##2
Alternatively, could have grouped by sex and calculated mean or looked at age distribution to find a better proxy than mean.
       
#3: Lifeboat
titanic_df$boat <- as.character(titanic_df$boat)
titanic_df$boat[grep("^$", titanic_df$boat)] <- "NA"
#Should I be converting boat to char first? "invalid factor level, NA generated"
#grep("^$", titanic_df$boat)

#4: Cabin
##Does it make sense to fill missing cabin numbers with a value?
##What does a missing value here mean?
#Either poor records of where they were located or maybe some passengers didn't have designated cabins

has_cabin_number <- as.numeric(titanic_df$cabin != "")
titanic_df <- cbind(titanic_df, has_cabin_number)

#5: Submit the project on Github
write.csv(titanic_df, ("/Users/jonathonthompson/Documents/data_science/sb_datawrangling_2/titanic_clean.csv"))



