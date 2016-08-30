library("dplyr")
library("tidyjson")
library("jsonlite")

edmunds_equip_id <- fromJSON("https://api.edmunds.com/api/vehicle/v2/styles/200477004/equipment?availability=standard&fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten = TRUE)

edmunds_equip_id <- as.data.frame(edmunds_equip_id, stringAsFactors = FALSE)

tbl_json(edmunds_equip_id, json.list = "equipment.attributes")

edmunds_equip <- vehicle_equip_j %>%
  gather_array %>%
  spread_values(equipment.id = jstring("equipment.id")) %>%
  enter_object("equipment.attributes") %>% gather_array %>%
  spread_values(
    equipment.key = jstring("equipment.attributes", "name"),
    equipment.value = jstring("equipment.attributes", "value"))
  

edmunds_equip_id <- select(edmunds_equip_id, equipment.id, equipment.attributes)

as

edmunds_equip <- edmunds_equip_id  %>% gather_array %>%
  spread_values(equipment.id = jstring("equipment.id")) %>%
  enter_object("equipment.attributes") %>% 
  gather_array %>%
  spread_values(equipment.key = jstring("name"), equipment.value = jstring("value"))

rm(vehicle_equip_j)  

determine_types(edmunds_equip_id)

select(equipment.id, equipment.key, equipment.value)


head(companies)


str(edmunds_equip_id)



##WORKING
raw_contents <- GET(url = "https://api.edmunds.com/api/vehicle/v2/styles/200477004/equipment?availability=standard&fmt=json&api_key=5gttt525w7ktadeqkytk2jez")
json_raw <- httr::content(raw_contents, type = "text")


edmunds_equip_tst <- json_raw %>% 
  enter_object("equipment") %>% 
  gather_array %>%
  spread_values(equipment.id = jstring("id")) %>%
  enter_object("attributes") %>% 
  gather_array %>%
  spread_values(
    attributes.name = jstring("name"),
    attributes.value = jstring("value")
  ) %>%
  select(equipment.id, attributes.name, attributes.value)


edmunds_equip_df <- as.data.frame(edmunds_equip_tst)
tbl_df(edmunds_equip_df)

edmunds_equip_full <- left_join(edmunds_equip_id, edmunds_equip_df, by = "equipment.id")




#8/27
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

#rm(edmunds_styles_df)
edmunds_styles_df <- data.frame(a=character(),
                                  stringsAsFactors=FALSE) 

for(i in 1:nrow(getaround_acura)) {
  ga_url <- print(paste0(url, tolower(getaround_acura$cars.make[i]),  "/", tolower(getaround_acura$cars.model[i]), "/", getaround_acura$cars.year[i], 
               path, api_key))
  ga_json_styles <- fromJSON(ga_url, flatten =  TRUE)
  ga_json_styles <- as.data.frame(ga_json_styles) 
  
  ga_json_styles$cars.make <- tolower(getaround_acura$cars.make[i])
  ga_json_styles$cars.model <- tolower(getaround_acura$cars.model[i])
  ga_json_styles$cars.year <- getaround_acura$cars.year[i]
  ga_json_styles$cars.api <- ga_url
  
  edmunds_styles_df <- bind_rows(edmunds_styles_df, ga_json_styles)
}


for(i in 1:nrow(getaround_acura)) {
  GET(url=(paste0(url, tolower(getaround_acura$cars.make[i]),  "/", tolower(getaround_acura$cars.model[i]), "/", getaround_acura$cars.year[i], 
               path, api_key)))
}


rm(raw.getaround)

raw.getaround <- GET(url = "https://api.edmunds.com/api/vehicle/v2/acura/rdx/2008/styles?state=used&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez" )
head(raw.getaround$content)
this.raw.content <- rawToChar(raw.getaround$content)
nchar(this.raw.content)
this.content <- fromJSON(this.raw.content)
class(this.content)
length(this.content)
this.content[[1]]

#turn into DF
this.content.df <- as.data.frame(this.content)
class(this.content.df)
dim(this.content.df)
head(this.content.df)



#8/27
######
#08/29

edmunds_styles_ids <- edmunds_styles_df %>%
  select(cars.year, cars.make, cars.model, styles.id) %>%
  group_by(cars.year, cars.make, cars.model) %>%
  arrange(styles.id) %>%
  slice(1:1) %>%
  arrange(cars.make, cars.model, cars.year) %>%
  filter(cars.make == "acura") #remove this for the full api call


#Creating Styles Details api calls
#https://api.edmunds.com/api/vehicle/v2/styles/200484357?view=full&fmt=json&api_key=5gttt525w7ktadeqkytk2jez

url_details <- "https://api.edmunds.com/api/vehicle/v2/styles/"
path_details <- "/equipment?availability=standard&fmt=json&api_key="
api_key <- "5gttt525w7ktadeqkytk2jez"

##Create empty dataframe to store api results
#rm(edmunds_styles_df)
edmunds_styles_details_df <- data.frame()


##For loop to create api url, import json, convert to df, add api info and add to existing df
for(i in 1:nrow(edmunds_styles_ids)) {
  ed_details_url <- print(paste0(url_details, edmunds_styles_ids$styles.id[i], path_details, api_key))
  ed_details_json <- GET(url = ed_details_url)
  ed_details_json <- httr::content(ed_details_json, type = "text")
  
  #Format json ##need to get a few more fields from here##
  ed_details_json <- ed_details_json %>% 
    enter_object("equipment") %>% 
    gather_array %>%
    spread_values(equipment.id = jstring("id")) %>%
    enter_object("attributes") %>% 
    gather_array %>%
    spread_values(
      attributes.name = jstring("name"),
      attributes.value = jstring("value")
    ) %>%
    select(equipment.id, attributes.name, attributes.value)
  
  #Add api call info
  ed_details_json$styles.id <- edmunds_styles_ids$styles.id[i]
  
  edmunds_styles_details_df <- bind_rows(edmunds_styles_details_df, ed_details_json)
}


#

for(i in 1:nrow(edmunds_styles_ids)) {
  ed_details_url <- print(paste0(url_details, edmunds_styles_ids$styles.id[i], path_details, api_key))
  ed_details_json <- fromJSON(url_details, flatten =  TRUE)
  ed_details_json <- as.data.frame(ed_details_json) 
  
  #Add api call info
  edmunds_styles_ids$styles.id <- edmunds_styles_ids$styles.id[i]
  
  edmunds_styles_details_df <- bind_rows(edmunds_styles_details_df, ed_details_json)
}

raw.ed_styles_details <- GET(url = "https://api.edmunds.com/api/vehicle/v2/styles/101418219?view=full&fmt=json&api_key=5gttt525w7ktadeqkytk2jez")
json_raw <- httr::content(raw.ed_styles_details, type = "text")

json_df <- as.data.frame(json_raw)


#Edmunds Styles Data
raw_contents <- GET(url = "https://api.edmunds.com/api/vehicle/v2/styles/101418219/equipment?availability=standard&fmt=json&api_key=5gttt525w7ktadeqkytk2jez")
json_raw <- httr::content(raw_contents, type = "text")

edmunds_equip_tst <- json_raw %>% 
  enter_object("equipment") %>% 
  gather_array %>%
  spread_values(equipment.id = jstring("id")) %>%
  enter_object("attributes") %>% 
  gather_array %>%
  spread_values(
    attributes.name = jstring("name"),
    attributes.value = jstring("value")
  ) %>%
  select(equipment.id, attributes.name, attributes.value)


edmunds_equip_df <- as.data.frame(edmunds_equip_tst)
edmunds_equip_full <- left_join(edmunds_equip_id, edmunds_equip_df, by = "equipment.id")




#08/29
#####






raw.getaround <- GET(url = "https://api.edmunds.com/api/vehicle/v2/toyota/yaris/2010/styles?state=used&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez")

edmunds_styles_api <- data.frame(ga_url = character())

#For loop to create api url, import json, convert to df, add api info and add to existing df
for(i in 1:nrow(getaround_mmy)) {
  ga_url <- (paste0(url, tolower(getaround_mmy$cars.make[i]),  "/", tolower(getaround_mmy$cars.model[i]), "/", getaround_mmy$cars.year[i], 
                         path, api_key))
}
  
  
str(ga_url)


vehicle_equip5 <- data.frame(matrix(unlist(vehicle_equip$equipment.attributes),nrow = 217, byrow = T), stringsAsFactors = FALSE)

vehicle_equip3 <- do.call(rbind.data.frame, vehicle_equip$equipment.attributes)

library(data.table)
vehicle_equip4 <- rbindlist(vehicle_equip$equipment.attributes)

unnest(vehicle_equip$equipment.attributes)



vehicle_equip[[27,"equipment.attributes"]]


vehicle_mazda <- fromJSON("https://api.edmunds.com/api/vehicle/v2/mazda/models?state=used&year=2013&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten = TRUE)
str(vehicle_equip)
vehicle_mazda <- as.data.frame(vehicle_mazda)
tbl_df(vehicle_style)



rm(equip_tst)

#add one df to another
biggerset <- bind_rows(bigset, vehicle_model)  


tbl_df(vehicle_years)

dim(years_json)
vehicle_years <- as.data.frame(vehicle_years)
years_json <- unnest(vehicle_years1$makes)

years_json1 <- data.frame(matrix(unlist(years_json$years),nrow = 361, byrow = T), stringsAsFactors = FALSE)
vehicle_years <- bind_cols(years_json, years_json1)








data.table_rbindlist

?combine()

class(years_json1)
class(years_json$years)


years_json1 <- lapply(years_json1$years, unlist)
years_tbl <- as.data.frame(years_flat)
tbl_df(years_tbl)



rm(years_flat)



#Write to .csv
write.csv(getaround, "getaround/getaround_api.csv")
write.csv(vehicle_model,"getaround/edmunds_vehicle_model_api.csv")

#get new cars and sort
getaround %>%
  filter(cars.year > 2010) %>%
  select(cars.year, cars.make, cars.model) %>%
  arrange(cars.year, cars.make, cars.model) %>%
  
  #get min and max daily prices
  getaround %>%
  filter(cars.year > 2010) %>%
  group_by(cars.year, cars.make, cars.model) %>%
  summarise(max_price = max(cars.price_daily, na.rm = TRUE), min_price = min(cars.price_daily, na.rm = TRUE)) %>%
  select(cars.year, cars.make, cars.model, max_price, min_price) %>%
  arrange(cars.year, cars.make, cars.model, max_price)

#get vehicle model data into df
vehicle_model[[6]]
ed_style_id <- as.data.frame(vehicle_model[[6]])



##
rm(edmunds)

vehicle_model[[6]][["id"]]

sapply(vehicle_model[[6]], 
       function(x) c(x$id, x$name, x$trim, x$submodel.body, x$submodel.modelName, x$submodel.niceName))


newnew <- lapply(
  vehicle_model[[6]], 
  function(x) c(x$id, x$name, x$trim, x$submodel.body, x$submodel.modelName, x$submodel.niceName))
newnew <- do.call(rbind, newnew)
newnew <- as.data.frame(newnew)

newnewish <- unlist(newnew)
newnewish <- as.data.frame(newnewish)

test <- unlist(vehicle_model)
test <- as.data.frame((test))

str(vehicle_model)
#####



getaround_mmy %>% 
  filter(getaround_mmy$cars.make == "mazda") %>%
  filter(grepl("5", getaround_mmy$cars.model))



mazda3 <- (filter(getaround_mmy, cars.make == "Mazda", (grepl("3", cars.model))))


mutate(getaround_mmy, cars.model = (ifelse(cars.make = "Mazda", (grepl("3", cars.model), "3" ))))

getaround_mmy %>%
  mutate()

?replace


gsub("3","3", getaround_mmy$cars.model)

filter(getaround_mmy, cars.make == "Mazda") %>%
  
  getaround_mmy$cars.model
refine_df$company[grepl("^ak", refine_df$company)] <- "akzo"
grepl("GTI", getaround_mmy$cars.model)
gsub("GTI", "_", getaround_mmy$cars.model, fixed = TRUE)










