install.packages("dplyr")
install.packages("tidyjson")
install.packages("jsonlite")
install.packages("tidyr")

library("dplyr")
library("tidyjson")
library("jsonlite")
library("tidyr")

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
    spread_values(equipment.id = jstring("id"),
                  equipment.type = jstring("equipmentType"),
                  equipment.name = jstring("name"),
                  engine.cr = jstring("compressionRatio"),
                  engine.cylinder = jstring("cylinder"),
                  engine.displacement = jstring("displacement")) %>%
    enter_object("attributes") %>% 
    gather_array %>%
    spread_values(
      attributes.name = jstring("name"),
      attributes.value = jstring("value")
    ) %>%
    select(equipment.id, equipment.type, equipment.name, attributes.name, attributes.value, engine.cr, engine.cylinder, engine.displacement)
  
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
#08/30

test_json <- fromJSON("https://api.edmunds.com/api/vehicle/v2/styles/101418219?view=full&fmt=json&api_key=5gttt525w7ktadeqkytk2jez")
test_json1 <- fromJSON("https://api.edmunds.com/api/vehicle/v2/styles/101418219?view=full&fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten = TRUE)

ed_styles_tst <- GET(url = "https://api.edmunds.com/api/vehicle/v2/styles/101418219?view=full&fmt=json&api_key=5gttt525w7ktadeqkytk2jez")
ed_styles_json <- httr::content(ed_styles_tst, type = "text")

#Format json ##need to get a few more fields from here##
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
                categories.vehicletype = jstring("categories", "vehicleType")
                ) %>%
  
  enter_object("engine") %>%
  spread_values(engine.cylinder = jstring("cylinder"),
                engine.displacement = jstring("displacement"),
                engine.horsepower = jstring("horsepower"),
                engine.rpm.hp = jstring("rpm","horsepower"),
                engine.torque = jstring("torque"),
                engine.rpm.tq = jstring("rpm","torque"))

###
prettify(ed_styles_json)
json_types(ed_styles_json)
###


##For loop to create api url, import json, convert to df, add api info and add to existing df
for(i in 1:nrow(edmunds_styles_ids)) {
  ed_details_url <- print(paste0(url_details, edmunds_styles_ids$styles.id[i], path_details, api_key))
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
    ) %>%
    select(equipment.id, equipment.type, equipment.name, attributes.name, attributes.value, engine.cr, engine.cylinder, engine.displacement)
  
  #Add api call info
  ed_details_json$styles.id <- edmunds_styles_ids$styles.id[i]
  
  edmunds_styles_details_df <- bind_rows(edmunds_styles_details_df, ed_details_json)
}

  
  engine.cr = jstring("compressionRatio"),
  engine.cylinder = jstring("cylinder"),
  engine.displacement = jstring("displacement")
  
  
  #Get search results from Getaround
  edmunds_styles_spec <- fromJSON("https://api.edmunds.com/api/vehicle/v2/styles/101418219?view=full&fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten = TRUE)
  
  ##Convert to a data frame
  edmunds_styles_spec <- as.data.frame(edmunds_styles_spec)  
  
  
#rm(edmunds_styles_details_df)
##08/30
######

######
#09/01
library("tidyr")  

str(edmunds_styles_details_df)
  
edmunds_styles_details_df$attributes.name <- gsub(" ", "_", edmunds_styles_details_df$attributes.name, fixed = TRUE)

ed_spread5 <- edmunds_styles_details_df %>%
  select(styles.id, attributes.name, attributes.value) %>%
  spread(attributes.name, attributes.value, drop = TRUE)
  
  spread(edmunds_styles_details_df, attributes.name, attributes.value, drop = TRUE) %>% select(styles.id, Epa_Interior_Volume)
\
  
  
##01/16##  
  #Creating Edmunds Ratings api calls
  #https://api.edmunds.com/api/vehicle/v2/grade/acura/ilx/2013?submodel=sedan&fmt=json&api_key=5gttt525w7ktadeqkytk2jez
  url_reviews <- "https://api.edmunds.com/api/vehicle/v2/grade/"
  path_reviews <- "&fmt=json&api_key="
  
  
  ##Create empty dataframe to store api results
  #rm(edmunds_reviews_df)
  edmunds_reviews_df <- data.frame()
  
  ##For loop to create api url, import json, convert to df, add api info and add to existing df
  for(i in 1:nrow(edmunds_styles_ids2)) {
    try(
      {review_url <- print(paste0(url_reviews, edmunds_styles_ids$cars.make[i],  "/", edmunds_styles_ids$cars.model[i], "/", edmunds_styles_ids$cars.year[i], 
                                  "?submodel=", edmunds_styles_ids$styles.submodel.niceName[i], path_reviews, api_key))
      review_json <- fromJSON(review_url, flatten =  TRUE)
      review_df <- as.data.frame(review_json)
      
      review_df <- unnest(review_df, ratings.subRatings)
      
      edmunds_reviews_df <- bind_rows(edmunds_reviews_df, review_df)}
    )
  } 
  
review_count <- distinct(select(edmunds_reviews_df, style.id, model.niceName, make.niceName,year.year ))

##01/16 add this logic into list##
edmunds_styles_ids2 <- distinct(select(edmunds_styles_df, styles.make.niceName, styles.model.niceName, styles.year.year, styles.submodel.niceName)) %>% 
  filter(styles.year.year >= 2013)
####


edmunds_styles_details_distinct <- distinct(select(edmunds_styles_details_df, equipment.type, equipment.name, attributes.name, attributes.value ))

###
str(edmunds_styles_details_df)

**WORKING ON THIS  
review_json  <- fromJSON("https://api.edmunds.com/api/vehicle/v2/grade/acura/ilx/2013?submodel=sedan&fmt=json&api_key=5gttt525w7ktadeqkytk2jez", flatten = TRUE)
review_df <- as.data.frame(review_json)

review_sub_df <- unnest(review_df, ratings.subRatings)

review_spread_main <- review_df %>%
  select(style.id, ratings.title, ratings.grade) %>%
  spread(ratings.title, ratings.grade)

review_spread_sub <- review_sub_df %>%
  select(style.id, sub.title, sub.grade) %>%
  spread(sub.title, sub.grade)

#Rename sub-category columns
colnames(review_sub_df)[22] <- "sub.title"
colnames(review_sub_df)[23] <- "sub.grade"
colnames(review_sub_df)[24] <- "sub.score"
colnames(review_sub_df)[25] <- "sub.summary"


review_spread_sub <- review_sub_df %>%
  unite(ratings.sub.title, ratings.title, sub.title) %>%
  select(style.id, ratings.sub.title, sub.grade) %>%
  spread(ratings.sub.title, sub.grade)

full_reviews <- merge(review_spread_main, review_spread_sub)

####
#Sample data sets
##Getaround
getaround_sample <- getaround %>% 
  select(cars.year, cars.make, cars.model, cars.car_name, cars.car_id, cars.distance)
write.csv(getaround_sample, file = paste0("getaround_sample_", format(Sys.Date(), "%Y%m%d"), ".csv"))

##Vehicle Style
ed_style_sample <- edmunds_styles_df %>%
  select(styles.id, styles.year.year, styles.make.niceName, styles.model.niceName, styles.trim, styles.submodel.niceName) %>%
  filter(styles.trim == 'Base')
write.csv(ed_style_sample, file = paste0("ed_style_sample_", format(Sys.Date(), "%Y%m%d"), ".csv"))

##Vehicle Equipment Basic
ed_basic_sample <- edmunds_styles_basic_df %>%
  select(styles.id, styles.drivewheels, styles.doors, categories.market, categories.vehiclesize, categories.vehiclestyle, engine.cylinder, engine.horsepower, engine.torque)
write.csv(ed_basic_sample, file = paste0("ed_basic_sample_", format(Sys.Date(), "%Y%m%d"), ".csv"))

##Vehicle Equipment Details
ed_detail_sample <- edmunds_styles_details_df %>%
  select(styles.id, attributes.name, attributes.value) %>%
  filter(attributes.name == 'Epa City Mpg' | attributes.name == 'Epa Combined Mpg' | attributes.name == 'Epa Interior Volume' | attributes.name == 'Max Cargo Capacity' | 
           attributes.name == 'Total Number Of Speakers' | attributes.name == 'Curb Weight') %>%
  spread(attributes.name, attributes.value)
write.csv(ed_detail_sample, file = paste0("ed_detail_sample_", format(Sys.Date(), "%Y%m%d"), ".csv"))

##Reviews
full_reviews <- merge(review_spread_main, review_spread_sub)
write.csv(full_reviews, file = paste0("edmunds_reviews_sample_", format(Sys.Date(), "%Y%m%d"), ".csv"))

####
getaround_2013 <- subset(getaround, cars.year >= 2013)
getaround_mmy_2013 <- subset(getaround_mmy, cars.year >= 2013)

edmunds_styles_ids_2013 <- subset(edmunds_styles_ids, cars.year >= 2013)
########
##01/17##
reviews_mmy <- distinct(select(edmunds_reviews_df, year.year, make.niceName, model.niceName)) 

###
##Summarize Getaround Data
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


#########
##01/24##
#########

#Set working directory
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/data_science/getaround")

#Load csv files
getaround <- read.csv("getaround_clean_20170116.csv")
edmunds_style <- read.csv("edmunds_styles_orig_20170116.csv")
edmunds_basic <- read.csv("edmunds_styles_basic_orig_20170116.csv")
edmunds_detail <- read.csv("edmunds_styles_details_orig_20170116.csv")


#Gather data needed for models
edmunds_style <- edmunds_style %>%
  select(styles.id, cars.make, cars.model, cars.year, styles.submodel.fuel, styles.submodel.tuner)  
  
edmunds_basic <- edmunds_basic %>%
  select(styles.id, styles.drivewheels, styles.doors, categories.market, categories.epaclass, categories.vehiclestyle, engine.horsepower)
  
model.data <- merge(edmunds_style, edmunds_basic) 

edmunds_detail <- edmunds_detail %>%
  select(styles.id, attributes.name, attributes.value) %>%
  filter(attributes.name == 'Manufacturer 0 60mph Acceleration Time (seconds)' | attributes.name == 'Epa City Mpg' | attributes.name == 'Epa Combined Mpg' | attributes.name == 'Epa Interior Volume' | attributes.name == 'Max Cargo Capacity' | 
           attributes.name == 'Total Number Of Speakers' | attributes.name == 'Curb Weight') %>%
  spread(attributes.name, attributes.value)

model.data <-  merge(model.data, edmunds_detail)


#Transform data
##Rename columns
colnames(model.data)[17] <- "zero_sixty"

##
model.data$zero_sixty <- as.numeric(as.character(model.data$zero_sixty))
model.data$power_weight <- (as.numeric(model.data$engine.horsepower) / as.numeric(model.data$`Curb Weight`))

str(model.data)

#Normalize Getaround distance
#for(i in 1:nrow(getaround)) {
 # getaround$dist_norm[i] <- 1 - (getaround$cars.distance[i] - min(getaround$cars.distance, na.rm = TRUE))/(max(getaround$cars.distance, na.rm = TRUE) - min(getaround$cars.distance, na.rm = TRUE))
#}

#Build Models
##Fun to drive
##rm(model.fun.output)
model.fun.output <- model.data %>%
  select(styles.id)
  
###0-60 second time (Lower time is better)
for(i in 1:nrow(model.data)) {
  #?Do I want to use average zero to 60 like this? titanic_df$age[is.na(titanic_df$age)] <- mean(titanic_df$age, na.rm = TRUE)
  #?Take 
  model.fun.output$zero_sixty[i] <- 1 - (model.data$zero_sixty[i] - min(model.data$zero_sixty, na.rm = TRUE))/(max(model.data$zero_sixty, na.rm = TRUE) - min(model.data$zero_sixty, na.rm = TRUE))
  model.fun.output$power_weight[i] <- (model.data$power_weight[i] - min(model.data$power_weight, na.rm = TRUE))/(max(model.data$power_weight, na.rm = TRUE) - min(model.data$power_weight, na.rm = TRUE))
  }

###Vehicle into binary
model.fun.output$market <- as.numeric(grepl("Performance", model.data$categories.market)|grepl("Exotic", model.data$categories.market)|grepl("Factory Tuner",model.data$categories.market))
model.fun.output$vehiclestyle <- as.numeric(grepl("Coupe", model.data$categories.vehiclestyle)|grepl("Convertible", model.data$categories.vehiclestyle))
model.fun.output$drivewheels <- as.numeric(grepl("all wheel drive", model.data$styles.drivewheels)|grepl("rear wheel drive", model.data$styles.drivewheels))


###Sum Score
model.fun.output$score <- (model.fun.output$zero_sixty + model.fun.output$power_weight + model.fun.output$market + model.fun.output$vehiclestyle + model.fun.output$drivewheels)

###########
#01/28
#model.fun.output$horsepower[i] <- (model.data$engine.horsepower[i] - min(model.data$engine.horsepower, na.rm = TRUE))/(max(model.data$engine.horsepower, na.rm = TRUE) - min(model.data$engine.horsepower, na.rm = TRUE))

#scale function
for(i in 1:nrow(model.data.test)) {
  model.fun.output.test$zero_scaleTF[i] <- scale(model.data.test$zero_sixty[i], center = TRUE, scale = FALSE)
  model.fun.output.test$zero_scaleFF[i] <- scale(model.data.test$zero_sixty[i], center = FALSE, scale = FALSE)
  model.fun.output.test$zero_scaleFT[i] <- scale(model.data.test$zero_sixty[i], center = FALSE, scale = TRUE)
  model.fun.output.test$zero_scaleFF[i] <- scale(model.data.test$zero_sixty[i], center = FALSE, scale = FALSE)
  model.fun.output.test$zero_scaleTT[i] <- scale(model.data.test$zero_sixty[i], center = TRUE, scale = TRUE)
  
  }


model.data.test <- model.data %>%
  select(zero_sixty) %>%
  filter(zero_sixty != "NA")

######
##adding columns
model.fun.output$score <- rowsum(x, model.fun.output, na.rm = TRUE)

model.fun.output <- model.fun.output %>%
  select()


x <- matrix(runif(100), ncol = 5)
group <- sample(1:8, 20, TRUE)
(xsum <- rowsum(x, group))

mutate(model.fun.output, model.fun.output$score = rowSums(model.fun.output[,2:6]))

##
has_cabin_number <- as.numeric(titanic_df$cabin != "")

|model.data$categories.market=="High-Performance"|model.data$categories.market=="Performance"|
  model.data$categories.market=="Exotic")


x <- mean(model.data$zero_sixty, na.rm = TRUE)
y <- min(model.data$zero_sixty, na.rm = TRUE)
z <- max(model.data$zero_sixty, na.rm = TRUE)


categories.vehiclestyle
Coupe,Convertible

plot(model.data$categories.vehiclestyle, model.data$zero_sixty)
###
#02/03

ggplot2()








