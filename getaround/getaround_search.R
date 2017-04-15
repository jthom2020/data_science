#Install and Load required packages
##install.packages("dplyr")
##install.packages("jsonlite")
##install.packages("tidyjson")
##install.packages("tidyr")
library("dplyr")
library("jsonlite")
library("tidyjson")
library("tidyr")

#Set working directory
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/data_science/getaround")

#rm(list=ls(all=TRUE))


#1.Load Raw Data
#Load csv files
getaround <- read.csv("getaround_clean_20170116.csv", stringsAsFactors=FALSE)
edmunds_style <- read.csv("edmunds_styles_orig_20170116.csv", stringsAsFactors=FALSE)
edmunds_basic <- read.csv("edmunds_styles_basic_orig_20170116.csv", stringsAsFactors=FALSE)
edmunds_detail <- read.csv("edmunds_styles_details_orig_20170116.csv", stringsAsFactors=FALSE)

#Gather data needed for models from each file
edmunds_style <- edmunds_style %>%
  select(styles.id, cars.make, cars.model, cars.year, styles.submodel.fuel, styles.submodel.tuner)  

edmunds_basic <- edmunds_basic %>%
  select(styles.id, styles.drivewheels, styles.doors, categories.market, categories.epaclass, categories.vehiclestyle, engine.horsepower)

model.data <- merge(edmunds_style, edmunds_basic) 

edmunds_detail <- edmunds_detail %>%
  select(styles.id, attributes.name, attributes.value) %>%
  filter(attributes.name %in% c('Epa City Mpg', 'Epa Combined Mpg','Manufacturer 0 60mph Acceleration Time (seconds)','Curb Weight','Overall Length','Wheelbase','Turning Diameter','Epa Interior Volume',
                                'Max Cargo Capacity','Cargo Capacity, Rear Seat Down Or Removed','Total Number Of Speakers', 'Watts','Cd Player','Cd Mp3 Playback','Usb Connection','Digital Audio Input',
                                'Memory Card Slot', 'Hard Drive', 'Premium Brand Audio System', 'Premium Brand Speakers', 'Subwoofer', 'Surround Audio', 'Radio Data System', 'Satellite Radio', 'Audio Controls On Steering Wheel',
                                'Speed Sensitive Volume Control', 'Roof Rack', '1st Row Seating Capacity', '2nd Row Seating Capacity', '3rd Row Seating Capacity','1st Row Upholstery', 'Navigation System', 'Heated Driver Seat',
                                'Heated Passenger Seat')) %>%
  spread(attributes.name, attributes.value)

model.data <-  merge(model.data, edmunds_detail)

#Cleanse Raw Data
##Reformat data types
model.data$zero_sixty <- as.numeric(model.data$'Manufacturer 0 60mph Acceleration Time (seconds)')
model.data$'Cargo Capacity, Rear Seat Down Or Removed' <- as.numeric(model.data$'Cargo Capacity, Rear Seat Down Or Removed')
model.data$'Curb Weight' <- as.numeric(model.data$'Curb Weight')
model.data$'Epa City Mpg' <- as.numeric(model.data$'Epa City Mpg')
model.data$'Epa Combined Mpg' <- as.numeric(model.data$'Epa Combined Mpg')
model.data$'Epa Interior Volume' <- as.numeric(model.data$'Epa Interior Volume')
model.data$'Max Cargo Capacity' <- as.numeric(model.data$'Max Cargo Capacity')
model.data$'Turning Diameter' <- as.numeric(model.data$'Turning Diameter')
model.data$'Total Number Of Speakers' <- as.numeric(model.data$'Total Number Of Speakers')
model.data$'Watts' <- as.numeric(model.data$'Watts')
model.data$'Wheelbase' <- as.numeric(model.data$'Wheelbase')
model.data$'Overall Length' <- as.numeric(model.data$'Overall Length')

##Fill in missing data points
##?Impute data points...revisit original dataset
#model.data$zero_sixty[is.na(model.data$zero_sixty)] <- mean(model.data$zero_sixty, na.rm = TRUE)

##Getaround Distance Factors
getaround$distance.factor[getaround$cars.distance <= .25] <- 1.00 #Local
getaround$distance.factor[getaround$cars.distance > .25 & getaround$cars.distance <= 1] <- .80 #Walkable
getaround$distance.factor[getaround$cars.distance > 1 & getaround$cars.distance <= 2]  <- .60 #Bikeable
getaround$distance.factor[getaround$cars.distance > 2 & getaround$cars.distance <= 5]  <- .40 #Uberable
getaround$distance.factor[getaround$cars.distance > 5] <- .20 #BARTable

#########################################################################################################

#2. Build Model Inputs
##rm(model.input)
model.input <- model.data %>%
  select(styles.id, cars.make, cars.model, cars.year)

#Normalize Data
model.input$cargo_norm <- as.numeric(scale(model.data$'Max Cargo Capacity', center = min(model.data$'Max Cargo Capacity', na.rm = TRUE), scale = diff(range(model.data$'Max Cargo Capacity', na.rm = TRUE))))
model.input$length_norm <- as.numeric(1 - (scale(model.data$'Overall Length', center = min(model.data$'Overall Length', na.rm = TRUE), scale = diff(range(model.data$'Overall Length', na.rm = TRUE)))))
model.input$mpg_city_norm <- as.numeric(scale(model.data$'Epa City Mpg', center = min(model.data$'Epa City Mpg', na.rm = TRUE), scale = diff(range(model.data$'Epa City Mpg', na.rm = TRUE))))
model.input$mpg_comb_norm <- as.numeric(scale(model.data$'Epa Combined Mpg', center = min(model.data$'Epa Combined Mpg', na.rm = TRUE), scale = diff(range(model.data$'Epa Combined Mpg', na.rm = TRUE))))
model.input$speaker_norm <- as.numeric(scale(model.data$'Total Number Of Speakers', center = min(model.data$'Total Number Of Speakers', na.rm = TRUE), scale = diff(range(model.data$'Total Number Of Speakers', na.rm = TRUE))))
model.input$turning_norm <- as.numeric(1 - (scale(model.data$'Turning Diameter', center = min(model.data$'Turning Diameter', na.rm = TRUE), scale = diff(range(model.data$'Turning Diameter', na.rm = TRUE)))))
model.input$zero_sixty_norm <- as.numeric(1 - (scale(model.data$zero_sixty, center = min(model.data$zero_sixty, na.rm = TRUE), scale = diff(range(model.data$zero_sixty, na.rm = TRUE)))))

##Binary Variables
###Market
model.input$is_crossover <- as.numeric(grepl('Crossover', model.data$categories.market))
model.input$is_exotic <- as.numeric(grepl('Exotic', model.data$categories.market))
model.input$is_luxury <- as.numeric(grepl('Luxury', model.data$categories.market))
model.input$is_performance <- as.numeric(grepl('Performance', model.data$categories.market))
model.input$is_tuner <- as.numeric(grepl('Factory Tuner', model.data$categories.market))
###Vehicle Style
model.input$is_coupe <- as.numeric(grepl('Coupe', model.data$categories.vehiclestyle))
model.input$is_convertible <- as.numeric(grepl('Convertible', model.data$categories.vehiclestyle))
model.input$is_hatchback <- as.numeric(grepl('2dr Hatchback', model.data$categories.vehiclestyle))
model.input$is_pickup <- as.numeric(grepl('Pickup', model.data$categories.vehiclestyle))
model.input$is_suv <- as.numeric(grepl('SUV', model.data$categories.vehiclestyle))
model.input$is_wagon <- as.numeric(grepl('Wagon', model.data$categories.vehiclestyle))
###Drive Wheels
model.input$is_awd <- as.numeric(grepl('all wheel drive', model.data$styles.drivewheels) | grepl('four wheel drive', model.data$styles.drivewheels))
model.input$is_rwd <- as.numeric(grepl('rear wheel drive', model.data$styles.drivewheels))
###Vehicle Features
model.input$has_steering <- as.numeric(grepl('*', model.data$'Audio Controls On Steering Wheel'))
model.input$has_cd <- as.numeric(grepl('*', model.data$'Cd Player'))
model.input$has_mp3 <- as.numeric(grepl('*', model.data$'Cd Mp3 Playback'))
model.input$has_digital_input <- as.numeric(grepl('*', model.data$'Digital Audio Input'))
model.input$has_hd <- as.numeric(grepl('*', model.data$'Hard Drive'))
model.input$has_mem_card <- as.numeric(grepl('*', model.data$'Memory Card Slot'))
model.input$has_prem_audio <- as.numeric(grepl('*', model.data$'Premium Brand Audio System'))
model.input$has_prem_speaker <- as.numeric(grepl('*', model.data$'Premium Brand Speakers'))
model.input$has_rds <- as.numeric(grepl('*', model.data$'Radio Data System'))
model.input$has_sat <- as.numeric(grepl('*', model.data$'Satellite Radio'))
model.input$has_speed_vol <- as.numeric(grepl('*', model.data$'Speed Sensitive Volume Control'))
model.input$has_sub <- as.numeric(grepl('*', model.data$'Subwoofer'))
model.input$has_surround <- as.numeric(grepl('*', model.data$'Surround Audio'))
model.input$has_usb <- as.numeric(grepl('*', model.data$'Usb Connection'))
model.input$has_heated_seat <- as.numeric(grepl('*', model.data$'Heated Driver Seat') | grepl('*', model.data$'Heated Passenger Seat'))
model.input$has_leather <- as.numeric(grepl('leather|suede', model.data$'1st Row Upholstery'))
model.input$has_rack <- as.numeric(grepl('*', model.data$'Roof Rack'))
model.input$has_navi <- as.numeric(grepl('*', model.data$'Navigation System'))
###Numerical Data to Binary
model.input$is_small <- as.numeric(model.data$'Overall Length' < 170) #170 inches total length of vehicle
model.input$is_newcar <- as.numeric(model.data$cars.year > 2015) #Flag cars newer than 2015 as "new"

#########################################################################################################

#3. Run Search Models
#Fun to Drive
##rm(model.fun)
model.fun <- model.input %>%
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.fun <- c(.40, .04, .16, .10, .06, .10, .04, .02,.08)

model.fun.inputs <-  model.input %>% select(zero_sixty_norm, is_performance, is_tuner, is_exotic, is_coupe, is_convertible, is_hatchback, is_awd, is_rwd) 
model.fun.weights <- data.frame(mapply(`*`, model.fun.inputs[1:ncol(model.fun.inputs)], weights.fun))
model.fun.weights$score <- rowSums(model.fun.weights[1:ncol(model.fun.weights)], na.rm = TRUE)
model.fun <- cbind(model.fun, model.fun.weights)

##Search Results
model.fun <- merge(getaround, model.fun, by  = c("cars.make","cars.model","cars.year"))
model.fun$score.dist <- (model.fun$distance.factor * model.fun$score)
model.fun <- (model.fun %>% arrange(desc(score), cars.distance))

#Run Errands
##rm(model.errands)
model.errands <- model.input %>%
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.errands <- c(.40, .05, .50, .05)

model.errands.inputs <-  model.input %>% select(mpg_city_norm, turning_norm, is_small, cargo_norm)
model.errands.weights <- data.frame(mapply(`*`, model.errands.inputs[1:ncol(model.errands.inputs)], weights.errands))
model.errands.weights$score <- rowSums(model.errands.weights[1:ncol(model.errands.weights)], na.rm = TRUE)
model.errands <- cbind(model.errands, model.errands.weights)

##Search Results
model.errands <- merge(getaround, model.errands, by  = c("cars.make","cars.model","cars.year"))
model.errands$score.dist <- (model.errands$distance.factor * model.errands$score)
model.errands <- (model.errands %>% arrange(desc(score), cars.distance))

#Audiophile
##rm(model.audio)
model.audio <- model.input %>%
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.audio <- c(0.35, 0.025, 0.05,  0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.025)

model.audio.inputs <-  model.input %>% select(speaker_norm, has_cd, has_mp3, has_usb, has_digital_input, has_mem_card, has_hd, has_prem_audio, has_prem_speaker, 
                                                has_sub, has_surround, has_rds, has_sat, has_steering, has_speed_vol)
model.audio.weights <- data.frame(mapply(`*`, model.audio.inputs[1:ncol(model.audio.inputs)], weights.audio))
model.audio.weights$score <- rowSums(model.audio.weights[1:ncol(model.audio.weights)], na.rm = TRUE)
model.audio <- cbind(model.audio, model.audio.weights)

##Search Results
model.audio <- merge(getaround, model.audio, by  = c("cars.make","cars.model","cars.year"))
model.audio$score.dist <- (model.audio$distance.factor * model.audio$score)
model.audio <- (model.audio %>% arrange(desc(score), cars.distance))

#Ski Trip
#rm(model.ski)
model.ski <- model.input %>%
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.ski <- c(.15, .05, .20, .20, .20, .20)

model.ski.inputs <-  model.input %>% select(mpg_comb_norm, is_crossover, is_suv, is_wagon, is_awd, has_rack)
model.ski.weights <- data.frame(mapply(`*`, model.ski.inputs[1:ncol(model.ski.inputs)], weights.ski))
model.ski.weights$score <- rowSums(model.ski.weights[1:ncol(model.ski.weights)], na.rm = TRUE)
model.ski <- cbind(model.ski, model.ski.weights)

##Search Results
model.ski <- merge(getaround, model.ski, by  = c("cars.make", "cars.model", "cars.year"))
model.ski$score.dist <- (model.ski$distance.factor * model.ski$score)
model.ski <- (model.ski %>% arrange(desc(score), cars.distance))

#Date Night
##rm(model.date)
model.date <- model.input %>% 
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.date <- c(.50, .10, .10, .10, .20)

model.date.inputs <-  model.input %>% select(is_luxury, has_leather, has_heated_seat, has_navi, speaker_norm)
model.date.weights <- data.frame(mapply(`*`, model.date.inputs[1:ncol(model.date.inputs)], weights.date))
model.date.weights$score <- rowSums(model.date.weights[1:ncol(model.date.weights)], na.rm = TRUE)
model.date <- cbind(model.date, model.date.weights)

##Search Results
model.date <- merge(getaround, model.date, by  = c("cars.make", "cars.model", "cars.year"))
model.date$score.dist <- (model.date$distance.factor * model.date$score)
model.date <- (model.date %>% arrange(desc(score), cars.distance))
