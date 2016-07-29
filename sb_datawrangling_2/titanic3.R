#Titanic
#rm(titanic_df)
#str(titanic_df)
#head(titanic_df)

#0:Load the data in RStudio
titanic_df <- read.csv("/Users/jonathonthompson/Documents/data_science/sb_datawrangling_2/titanic_original.csv")
#titanic_df <- read.csv("https://github.com/jthom2020/data_science/blob/master/sb_datawrangling_2/titanic_original.csv")


#1: Port of embarkation
titanic_df$embarked <- gsub("^$", "S", titanic_df$embarked)
##titanic_df$embarked[grep("^$", titanic_df$embarked)] <- "S" 

#2: Age
titanic_df$age[is.na(titanic_df$age)] <- mean(titanic_df$age, na.rm = TRUE)
#age <- mean(titanic_df$age, na.rm = TRUE)
#grep("^$", titanic_df$age)
#filter(titanic_df, is.na(age))

#3: Lifeboat
titanic_df$boat <- as.character(titanic_df$boat)
titanic_df$boat[grep("^$", titanic_df$boat)] <- "NA"
#Should I be converting boat to char first? "invalid factor level, NA generated"
#grep("^$", titanic_df$boat)

#4: Cabin
##Does it make sense to fill missing cabin numbers with a value?
##What does a missing value here mean?

has_cabin_number <- as.numeric(titanic_df$cabin != "")
titanic_df <- cbind(titanic_df, has_cabin_number)

#5: Submit the project on Github
#write.csv(titanic_df, ("https://github.com/jthom2020/data_science/blob/master/sb_datawrangling_2/titanic_clean.csv"))
write.csv(titanic_df, ("/Users/jonathonthompson/Documents/data_science/sb_datawrangling_2/titanic_clean.csv"))

