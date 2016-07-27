#install packages and libraries
#install.packages("jsonlite")
#install.package("RJSONIO")
#install.packages("tidyjson")

library("jsonlite")


#Get search results from Getaround
getaround <- fromJSON("https://index.getaround.com/v1.0/search?product=web&uid=100005837281185&user_lat=37.7717185&user_lng=-122.44389289999998&viewport=37.514591%2C-122.644614%2C38.028441%2C-122.243172&properties=car_id,car_name,car_photo,carkit_enabled,distance,latitude,longitude,make,model,price_daily,price_hourly,price_weekly,total_price,timezone,year,dedicated_parking&sort=best&page_sort=magic&page_size=1000")
vehicle_model <- fromJSON("https://api.edmunds.com/api/vehicle/v2/honda/fit?state=used&year=2015&view=basic&fmt=json&api_key=5gttt525w7ktadeqkytk2jez")

#Convert to a data frame
getaround <- as.data.frame(getaround)
vehicle_model <- as.data.frame(vehicle_model)

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


