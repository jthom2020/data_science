#Install and Load required packages
##install.packages("dplyr")
##install.packages("httr")
##install.packages("jsonlite")
##install.packages("tidyjson")
##install.packages("tidyr")
library("dplyr")
library("httr")
library("jsonlite")
library("tidyjson")
library("tidyr")
options(stringsAsFactors = FALSE)

#rm(list=ls(all=TRUE))

#Set working directory
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/data_science/getaround")

#1. Getaround Data
#Get search results from Getaround
##Using my login info and location at Ashbury Street, San Francisco, CA 94117
getaround <- ?fromJSON("https://index.getaround.com/v1.0/search?product=web&uid=100005837281185&user_lat=37.7717185&user_lng=-122.44389289999998&viewport=37.514591%2C-122.644614%2C38.028441%2C-122.243172&properties=car_id,car_name,car_photo,carkit_enabled,distance,latitude,longitude,make,model,price_daily,price_hourly,price_weekly,total_price,timezone,year,dedicated_parking&sort=best&page_sort=magic&page_size=5000")

##Convert to a data frame
getaround <- as.data.frame(getaround)
write.csv(getaround, file = paste0("getaround_orig_", format(Sys.Date(), "%Y%m%d"), ".csv"))

#Clean Getaround Data
##Make lowercase and replace spaces with "-"
getaround$cars.make <- tolower(getaround$cars.make)
getaround$cars.make <- gsub(" ", "-", getaround$cars.make, fixed = TRUE)
getaround$cars.model <- tolower(getaround$cars.model)
getaround$cars.model <- gsub(" ", "-", getaround$cars.model, fixed = TRUE)

##Fix models
##Mazda - Model 3, 5, 6 fixes
  getaround$cars.model[getaround$cars.make == "mazda" & grepl("mazda3", getaround$cars.model)] <- "3"
  getaround$cars.model[getaround$cars.make == "mazda" & grepl("mazdaspeed3", getaround$cars.model)] <- "mazdaspeed-3"
  getaround$cars.model[getaround$cars.make == "mazda" & grepl("mazda5", getaround$cars.model)] <- "5"
  getaround$cars.model[getaround$cars.make == "mazda" & grepl("mazda6", getaround$cars.model)] <- "6"
##Volkswagen - GTI/Golf-GTI fixes
  getaround$cars.model[getaround$cars.make == "volkswagen" & (getaround$cars.year == "2015"|getaround$cars.year == "2016") & grepl("gti", getaround$cars.model)] <- "golf-gti"

##Export Clean Getaround data
write.csv(getaround, file = paste0("getaround_clean_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  
##Get distinct list of year/make/model
getaround_mmy <- distinct(select(getaround, cars.year, cars.make, cars.model))  #%>%
  #filter(cars.make == "acura") ##remove this for the full api call


#2. Edmunds Data
#Creating Styles api calls
#https://api.edmunds.com/api/vehicle/v2/honda/pilot/2010/styles?state=used&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez
url_styles_id <- "https://api.edmunds.com/api/vehicle/v2/"
path_styles_id <- "/styles?state=used&view=basic&fmt=json&api_key="
api_key <- "5gttt525w7ktadeqkytk2jez"

##Create empty dataframe to store api results
edmunds_styles_df <- data.frame()

##For loop to create api url, import json, convert to df, add api info and add to existing df
for(i in 1:nrow(getaround_mmy)) {
  try(
  {ga_url <- print(paste0(url_styles_id, getaround_mmy$cars.make[i],  "/", getaround_mmy$cars.model[i], "/", getaround_mmy$cars.year[i], 
                         path_styles_id, api_key))
  ga_json_styles <- fromJSON(ga_url, flatten =  TRUE)
  ga_json_styles <- as.data.frame(ga_json_styles) 
  
  #Add api call info
  ga_json_styles$cars.make <- getaround_mmy$cars.make[i]
  ga_json_styles$cars.model <- getaround_mmy$cars.model[i]
  ga_json_styles$cars.year <- getaround_mmy$cars.year[i]
  ga_json_styles$cars.api <- ga_url
  
  edmunds_styles_df <- bind_rows(edmunds_styles_df, ga_json_styles)}
  )
}

##Export raw Edmunds styles data
write.csv(edmunds_styles_df, file = paste0("edmunds_styles_orig_", format(Sys.Date(), "%Y%m%d"), ".csv"))


#Filter down to a single, unique style_id per Make/Model/Year
edmunds_styles_ids <- edmunds_styles_df %>%
  #filter(styles.trim == 'Base') %>% ##Not a good hit rate, this is a limitation of the analysis
  select(styles.id, cars.year, cars.make, cars.model, styles.submodel.niceName) %>%
  group_by(cars.year, cars.make, cars.model) %>%
  arrange(styles.id) %>%
  slice(1:1) %>%
  arrange(cars.make, cars.model, cars.year) #%>% 
  #filter(cars.make == "acura") ##remove this for the full api call


#Creating Styles Details api calls for STANDARD Equipment
##https://api.edmunds.com/api/vehicle/v2/styles/101418219/equipment?availability=standard&fmt=json&api_key=5gttt525w7ktadeqkytk2jez
url_details <- "https://api.edmunds.com/api/vehicle/v2/styles/"
path_details <- "/equipment?availability=standard&fmt=json&api_key="

##Create empty dataframe to store api results
edmunds_styles_details_df <- data.frame()

##For loop to create api url, import json, convert to df, add api info and add to existing df
for(i in 1:nrow(edmunds_styles_ids)) {
  try(
  {ed_details_url <- print(paste0(url_details, edmunds_styles_ids$styles.id[i], path_details, api_key))
  ed_details_json <- GET(url = ed_details_url)
  ed_details_json <- httr::content(ed_details_json, type = "text")
  
  #Format json ##need to get a few more fields from here##
  ed_details_json <- ed_details_json %>% 
    enter_object("equipment") %>% 
    gather_array %>%
    spread_values(equipment.id = jstring("id"),
                  equipment.type = jstring("equipmentType"),
                  equipment.name = jstring("name")) %>%
    enter_object("attributes") %>% 
    gather_array %>%
    spread_values(
      attributes.name = jstring("name"),
      attributes.value = jstring("value")
    ) 
  
  #Add api call info
  ed_details_json$styles.id <- edmunds_styles_ids$styles.id[i]
  
  edmunds_styles_details_df <- bind_rows(edmunds_styles_details_df, ed_details_json)}
)}

##Export raw Edmunds styles details data
write.csv(edmunds_styles_details_df, file = paste0("edmunds_styles_details_orig_", format(Sys.Date(), "%Y%m%d"), ".csv"))


#Creating Styles Basics api calls
##"https://api.edmunds.com/api/vehicle/v2/styles/101418219?view=full&fmt=json&api_key=5gttt525w7ktadeqkytk2jez"
url_styles <- "https://api.edmunds.com/api/vehicle/v2/styles/"
path_styles <- "?view=full&fmt=json&api_key="

##Create empty dataframe to store api results
edmunds_styles_basic_df <- data.frame()

##For loop to create api url, import/ json, convert to df, add api info and add to existing df
for(i in 1:nrow(edmunds_styles_ids)) {
  try(
  {ed_styles_url <- print(paste0(url_styles, edmunds_styles_ids$styles.id[i], path_styles, api_key))
  ed_styles_json <- GET(url = ed_styles_url)
  ed_styles_json <- httr::content(ed_styles_json, type = "text")

ed_styles_json_df <- ed_styles_json %>%
  
  spread_values(styles.id = jstring("id"),
                styles.trim = jstring("trim"),
                styles.drivewheels = jstring("drivenWheels"),
                styles.doors = jstring("numOfDoors")) %>%
  
  spread_values(categories.market = jstring("categories", "market"),
                categories.epaclass = jstring("categories", "EPAClass"),
                categories.vehiclesize = jstring("categories", "vehicleSize"),
                categories.bodytype = jstring("categories", "primaryBodyType"),
                categories.vehiclestyle = jstring("categories", "vehicleStyle"),
                categories.vehicletype = jstring("categories", "vehicleType")) %>%
  
  enter_object("engine") %>%
  spread_values(engine.cylinder = jstring("cylinder"),
                engine.displacement = jstring("displacement"),
                engine.horsepower = jstring("horsepower"),
                engine.rpm.hp = jstring("rpm","horsepower"),
                engine.torque = jstring("torque"),
                engine.rpm.tq = jstring("rpm","torque"))

edmunds_styles_basic_df <- bind_rows(edmunds_styles_basic_df, ed_styles_json_df)}
)}

##Export data
write.csv(edmunds_styles_basic_df, file = paste0("edmunds_styles_basic_orig_", format(Sys.Date(), "%Y%m%d"), ".csv"))

##!NOT USING IN THIS ANALYSIS!##
#Creating Edmunds Ratings api calls
#https://api.edmunds.com/api/vehicle/v2/grade/acura/ilx/2013?submodel=sedan&fmt=json&api_key=5gttt525w7ktadeqkytk2jez
#url_reviews <- "https://api.edmunds.com/api/vehicle/v2/grade/"
#path_reviews <- "&fmt=json&api_key="

#edmunds_styles_ids_2013 <- distinct(select(edmunds_styles_df, styles.make.niceName, styles.model.niceName, styles.year.year, styles.submodel.niceName)) %>% 
#  filter(styles.year.year >= 2013) %>% #Only available for 2013 and newer models
#  arrange(styles.make.niceName, styles.model.niceName, styles.year.year, styles.submodel.niceName)

##Create empty dataframe to store api results
##rm(edmunds_reviews_df)
#edmunds_reviews_df <- data.frame()

##For loop to create api url, import json, convert to df, add api info and add to existing df
#for(i in 1:nrow(edmunds_styles_ids_2013)) {
#  try(
#    {review_url <- print(paste0(url_reviews, edmunds_styles_ids_2013$styles.make.niceName[i],  "/", edmunds_styles_ids_2013$styles.model.niceName[i], "/", 
#                                edmunds_styles_ids_2013$styles.year.year[i], "?submodel=", edmunds_styles_ids_2013$styles.submodel.niceName[i], path_reviews, api_key))
#    review_json <- fromJSON(review_url, flatten =  TRUE)
#    review_df <- as.data.frame(review_json)
#    
#    review_df <- unnest(review_df, ratings.subRatings)
#    
#    edmunds_reviews_df <- bind_rows(edmunds_reviews_df, review_df)}
#  )
#} 
#
##Export raw Edmunds Reviews data
#write.csv(edmunds_reviews_df, file = paste0("edmunds_reviews_df_", format(Sys.Date(), "%Y%m%d"), ".csv"))
