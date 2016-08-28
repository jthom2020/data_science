library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)
####
##08/27##
library(dplyr)
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

getaround_acura <- filter(getaround_mmy, cars.make == "Acura")
str(getaround_acura)

#Creating Styles api calls
#https://api.edmunds.com/api/vehicle/v2/honda/pilot/2010/styles?state=used&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez

url <- "https://api.edmunds.com/api/vehicle/v2/"
path <- "/styles?state=used&view=basic&fmt=json&api_key="
api_key <- "5gttt525w7ktadeqkytk2jez"

#Create empty dataframe to store api results
#rm(edmunds_styles_df)
edmunds_styles_df <- data.frame()

#For loop to create api url, import json, convert to df, add api info and add to existing df
for(i in 1:nrow(getaround_acura)) {
  ga_url <- print(paste0(url, tolower(getaround_acura$cars.make[i]),  "/", tolower(getaround_acura$cars.model[i]), "/", getaround_acura$cars.year[i], 
                         path, api_key))
  ga_json_styles <- fromJSON(ga_url, flatten =  TRUE)
  ga_json_styles <- as.data.frame(ga_json_styles) 

  #Add api call info
  ga_json_styles$cars.make <- tolower(getaround_acura$cars.make[i])
  ga_json_styles$cars.model <- tolower(getaround_acura$cars.model[i])
  ga_json_styles$cars.year <- getaround_acura$cars.year[i]
  ga_json_styles$cars.api <- ga_url
  
  
  edmunds_styles_df <- bind_rows(edmunds_styles_df, ga_json_styles)
}

write.csv(edmunds_styles_df, file = paste0("edmunds_styles_orig_", format(Sys.Date(), "%Y%m%d%I%M"), ".csv"))
##08/27##
####

?strpdate

####
raw.result.check <- GET(url = url)



make <- "honda"
model <- 
year <- 



edmunds_link <- paste0("https://api.edmunds.com/api/vehicle/v2/styles/200477004/equipment?availability=standard&fmt=json&api_key=", api_key)




vehicle_equip <- fromJSON("https://api.edmunds.com/api/vehicle/v2/styles/200477004/equipment?availability=standard&fmt=json&api_key=5gttt525w7ktadeqkytk2jez")

#https://api.edmunds.com/api/vehicle/v2/honda/pilot/2010/styles?state=used&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez


edmund_makes<- fromJSON("https://api.edmunds.com/api/vehicle/v2/makes?fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten)
edmund_makes <- as.data.frame(edmund_makes)
tbl_df(edmund_makes)















