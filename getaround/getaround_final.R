
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)
####
##08/27##
library(dplyr)
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

#install packages and libraries
library("jsonlite")
library("tidyr")
library("dplyr")

rm(list=ls(all=TRUE))

#Get search results from Getaround
getaround <- fromJSON("https://index.getaround.com/v1.0/search?product=web&uid=100005837281185&user_lat=37.7717185&user_lng=-122.44389289999998&viewport=37.514591%2C-122.644614%2C38.028441%2C-122.243172&properties=car_id,car_name,car_photo,carkit_enabled,distance,latitude,longitude,make,model,price_daily,price_hourly,price_weekly,total_price,timezone,year,dedicated_parking&sort=best&page_sort=magic&page_size=5000")

#Convert to a data frame
getaround <- as.data.frame(getaround)
write.csv(getaround, file = paste0("getaround_orig_", format(Sys.Date(), "%Y%m%d%I%M"), ".csv"))

#Getaround Data
##Get distinct list of year/make/model
getaround_mmy <- distinct(select(getaround, cars.year, cars.make, cars.model))
getaround_my <- distinct(select(getaround, cars.year, cars.make))
getaround_make <- distinct(select(getaround, cars.make))
getaround_model <- distinct(select(getaround, cars.model))
getaround_year <- distinct(select(getaround, cars.year))

#Clean Getaround Data
#Replace spaces with _
getaround_mmy$cars.make <- tolower(getaround_mmy$cars.make)
getaround_mmy$cars.model <- tolower(getaround_mmy$cars.model)
getaround_mmy$cars.model <- gsub(" ", "-", getaround_mmy$cars.model, fixed = TRUE)

#Fix models
##Acura - ok
##Audi - ok
##BMW - need "-"
##Buick - ok
##Cadillac - ok
##Chevrolet - ok
##Chrysler - ok
##Dodge - ok
##FIAT - ok
##Ford - ok
##GMC - ok
##Honda - ok
##Hyundai - ok
##Infiniti - ok
##Jeep - ok
##Kia - ok
##Lexus - ok
##Lincoln - ok
##Mazda - Model 3, 5, 6, Miata fixes
##Mercedes-Benz - ok
##Mercury - ok
##Mini - ok
##Mitsubishi - ok
##Nissan - ok
##Porsche - ok

##Mazda
getaround_mmy$cars.model[getaround_mmy$cars.make == "mazda" & grepl("3", getaround_mmy$cars.model)] <- "3"
getaround_mmy$cars.model[getaround_mmy$cars.make == "mazda" & grepl("mazda5", getaround_mmy$cars.model)] <- "5"
getaround_mmy$cars.model[getaround_mmy$cars.make == "mazda" & grepl("mazda6", getaround_mmy$cars.model)] <- "6"
getaround_mmy$cars.model[getaround_mmy$cars.make == "mazda" & grepl("mx-5 miata", getaround_mmy$cars.model)] <- "mx-5-miata"

##VW 
getaround_mmy$cars.model[getaround_mmy$cars.make == "volkswagen" & grepl("gti", getaround_mmy$cars.model)] <- "golf-gti"

###


getaround_mmy_test <- filter(getaround_mmy, cars.make == "porsche")


#Creating Styles api calls
#https://api.edmunds.com/api/vehicle/v2/honda/pilot/2010/styles?state=used&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez

url <- "https://api.edmunds.com/api/vehicle/v2/"
path <- "/styles?state=used&view=basic&fmt=json&api_key="
api_key <- "5gttt525w7ktadeqkytk2jez"

#Create empty dataframe to store api results
#rm(edmunds_styles_df)
edmunds_styles_df <- data.frame()

#For loop to create api url, import json, convert to df, add api info and add to existing df
for(i in 1:nrow(getaround_mmy_test)) {
  ga_url <- print(paste0(url, tolower(getaround_mmy_test$cars.make[i]),  "/", tolower(getaround_mmy_test$cars.model[i]), "/", getaround_mmy_test$cars.year[i], 
                         path, api_key))
  ga_json_styles <- fromJSON(ga_url, flatten =  TRUE)
  ga_json_styles <- as.data.frame(ga_json_styles) 
  
  #Add api call info
  ga_json_styles$cars.make <- tolower(getaround_mmy_test$cars.make[i])
  ga_json_styles$cars.model <- tolower(getaround_mmy_test$cars.model[i])
  ga_json_styles$cars.year <- getaround_mmy_test$cars.year[i]
  ga_json_styles$cars.api <- ga_url
  
  edmunds_styles_df <- bind_rows(edmunds_styles_df, ga_json_styles)
}


write.csv(edmunds_styles_df, file = paste0("edmunds_styles_orig_2015_", format(Sys.Date(), "%Y%m%d%I%M"), ".csv"))
#######



  getaround_mmy %>% 
  filter(getaround_mmy$cars.make == "mazda") %>%
  filter(grepl("5", getaround_mmy$cars.model))
  


mazda3 <- (filter(getaround_mmy, cars.make == "Mazda", (grepl("3", cars.model))))


mutate(getaround_mmy_test, cars.model = (ifelse(cars.make = "Mazda", (grepl("3", cars.model), "3" ))))

getaround_mmy_test %>%
  mutate()

?replace


  gsub("3","3", getaround_mmy$cars.model)

filter(getaround_mmy, cars.make == "Mazda") %>%

getaround_mmy$cars.model
refine_df$company[grepl("^ak", refine_df$company)] <- "akzo"
grepl("GTI", getaround_mmy_test$cars.model)
gsub("GTI", "_", getaround_mmy$cars.model, fixed = TRUE)







