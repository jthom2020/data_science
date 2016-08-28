#install packages and libraries
library("jsonlite")
library("tidyr")
library("dplyr")

#Get search results from Getaround
getaround <- fromJSON("https://index.getaround.com/v1.0/search?product=web&uid=100005837281185&user_lat=37.7717185&user_lng=-122.44389289999998&viewport=37.514591%2C-122.644614%2C38.028441%2C-122.243172&properties=car_id,car_name,car_photo,carkit_enabled,distance,latitude,longitude,make,model,price_daily,price_hourly,price_weekly,total_price,timezone,year,dedicated_parking&sort=best&page_sort=magic&page_size=5000")
vehicle_model <- fromJSON("https://api.edmunds.com/api/vehicle/v2/honda/fit?state=used&year=2015&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten = TRUE)
vehicle_years <- fromJSON("https://api.edmunds.com/api/vehicle/v2/makes?state=used&year=2014&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten = TRUE)


#Convert to a data frame
getaround <- as.data.frame(getaround)
vehicle_model <- as.data.frame(vehicle_model)

#Getaround Data
##Get distinct list of year/make/model
getaround_mmy <- distinct(select(getaround, cars.year, cars.make, cars.model))
getaround_my <- distinct(select(getaround, cars.year, cars.make))
getaround_make <- distinct(select(getaround, cars.make))
getaround_model <- distinct(select(getaround, cars.model))
getaround_year <- distinct(select(getaround, cars.year))


#Edmunds Vehicle Model Data
##Unnest and rename styles nested df
styles_json <- unnest(vehicle_model_flat$years)
colnames(styles_json)[1] <- "years.id"
colnames(styles_json)[3] <- "styles.id"

##Join back with vehicles
vehicle_model <- left_join(vehicle_model_flat, styles_json, by = "years.id")

##Start cleaning up
vehicle_model <- separate(vehicle_model, id, c("make","model"))


#Edmunds Vehicle Year Data
##http://stackoverflow.com/questions/4227223/r-list-to-data-frame
years_json1 <- data.frame(matrix(unlist(years_json$years),nrow = 361, byrow = T), stringsAsFactors = FALSE)
vehicle_years <- bind_cols(years_json, years_json1)


#Edmunds Styles Data
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
edmunds_equip_full <- left_join(edmunds_equip_id, edmunds_equip_df, by = "equipment.id")






