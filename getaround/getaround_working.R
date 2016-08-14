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

















vehicle_equip5 <- data.frame(matrix(unlist(vehicle_equip$equipment.attributes),nrow = 217, byrow = T), stringsAsFactors = FALSE)

vehicle_equip3 <- do.call(rbind.data.frame, vehicle_equip$equipment.attributes)

library(data.table)
vehicle_equip4 <- rbindlist(vehicle_equip$equipment.attributes)

unnest(vehicle_equip$equipment.attributes)



vehicle_equip[[27,"equipment.attributes"]]


vehicle_style <- fromJSON("https://api.edmunds.com/api/vehicle/v2/styles/200477004?view=full&fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten = TRUE)
str(vehicle_equip)
vehicle_style <- as.data.frame(vehicle_style)
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



