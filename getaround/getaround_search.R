#Install and Load required packages
##install.packages("dplyr")
##install.packages("jsonlite")
##install.packages("tidyjson")
##install.packages("tidyr")
library("dplyr")
library("jsonlite")
library("tidyjson")
library("tidyr")

#rm(list=ls(all=TRUE))

#Set working directory
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/data_science/getaround")


#1.Load Raw Data
#Load csv files
getaround <- read.csv("getaround_clean_20170116.csv", stringsAsFactors=FALSE)
edmunds_style <- read.csv("edmunds_styles_orig_20170116.csv", stringsAsFactors=FALSE)
edmunds_basic <- read.csv("edmunds_styles_basic_orig_20170116.csv", stringsAsFactors=FALSE)
edmunds_detail <- read.csv("edmunds_styles_details_orig_20170116.csv", stringsAsFactors=FALSE)
edmunds_detail_all <- read.csv("edmunds_styles_details_orig_20170116.csv", stringsAsFactors=FALSE) #Remove later

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
##Rename columns
#colnames(model.data)[23] <- "zero_sixty"

##Reformat data types
##? Can I do this in all one shot?
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
getaround$distance.factor[getaround$cars.distance > 5] <- .20 #Bartable

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
weights.fun <- c(.40, .04,.16,.10, .06,.10,.04, .02,.08)

model.fun$zero_sixty_norm <- (model.input$zero_sixty_norm * weights.fun[1])
model.fun$is_performance <- (model.input$is_performance * weights.fun[2])
model.fun$is_tuner <- (model.input$is_tuner * weights.fun[3])
model.fun$is_exotic <- (model.input$is_exotic * weights.fun[4])
model.fun$is_coupe <- (model.input$is_coupe * weights.fun[5])
model.fun$is_convertible <- (model.input$is_convertible * weights.fun[6])
model.fun$is_hatchback <- (model.input$is_hatchback * weights.fun[7])
model.fun$is_awd <- (model.input$is_awd * weights.fun[8])
model.fun$is_rwd <- (model.input$is_rwd * weights.fun[9])
model.fun$score <- rowSums(model.fun[4:12], na.rm = TRUE)

##Search Results
model.fun <- merge(getaround, model.fun, by  = c("cars.make","cars.model","cars.year"))
model.fun$score.dist <- (model.fun$distance.factor * model.fun$score)
results.fun <- distinct(rbind((model.fun %>% arrange(desc(score.dist)) %>% slice(1:10)), (model.fun %>% arrange(desc(score)) %>% slice(1:10))))

#Run Errands
##rm(model.errands)
model.errands <- model.input %>%
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.errands <- c(.05, .05, .50, .40)

model.errands$mpg_city_norm <- ((model.input$mpg_city_norm * weights.errands[1]))
model.errands$turning_norm <- ((model.input$turning_norm * weights.errands[2]))
model.errands$is_small <- ((model.input$is_small * weights.errands[3]))
model.errands$cargo_norm <- ((model.input$cargo_norm * weights.errands[4]))
model.errands$score <- rowSums(model.input[4:7], na.rm = TRUE)

##Search Results
model.errands <- merge(getaround, model.errands, by  = c("cars.make","cars.model","cars.year"))
model.errands$score.dist <- (model.errands$distance.factor * model.errands$score)
results.errands <- distinct(rbind((model.errands %>% arrange(desc(score.dist)) %>% slice(1:10)), (model.errands %>% arrange(desc(score)) %>% slice(1:10))))

#Audiophile
##rm(model.audio)
model.audio <- model.input %>%
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.audiophile <- c(0.35, 0.025, 0.05,  0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.025)

model.audio$speaker_norm <- ((model.input$speaker_norm * weights.audiophile[1]))
model.audio$has_cd <- ((model.input$has_cd * weights.audiophile[2]))
model.audio$has_mp3 <- ((model.input$has_mp3 * weights.audiophile[3]))
model.audio$has_usb <- ((model.input$has_usb * weights.audiophile[4]))
model.audio$has_digital_input <-  ((model.input$has_digital_input * weights.audiophile[5]))
model.audio$has_mem_card <-  ((model.input$has_mem_card * weights.audiophile[6]))
model.audio$has_hd <-  ((model.input$has_hd  * weights.audiophile[7]))
model.audio$has_prem_audio <-  ((model.input$has_prem_audio * weights.audiophile[8]))
model.audio$has_prem_speaker <-  ((model.input$has_prem_speaker * weights.audiophile[9]))
model.audio$has_sub <-  ((model.input$has_sub * weights.audiophile[10]))
model.audio$has_surround <-  ((model.input$has_surround * weights.audiophile[11]))
model.audio$has_rds <-  ((model.input$has_rds * weights.audiophile[12]))
model.audio$has_sat <-  ((model.input$has_sat * weights.audiophile[13]))
model.audio$has_steering <-  ((model.input$has_steering * weights.audiophile[14]))
model.audio$has_speed_vol <-  ((model.input$has_speed_vol * weights.audiophile[15]))
model.audio$score <- rowSums(model.audio[4:18], na.rm = TRUE)

##Search Results
model.audio <- merge(getaround, model.audio, by  = c("cars.make","cars.model","cars.year"))
model.audio$score.dist <- (model.audio$distance.factor * model.audio$score)
results.audio <- distinct(rbind((model.audio %>% arrange(desc(score.dist)) %>% slice(1:10)), (model.audio %>% arrange(desc(score)) %>% slice(1:10))))

#Ski Trip
#rm(model.ski)
model.ski <- model.input %>%
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.ski <- c(.15, .05, .20, .20, .20, .20)

model.ski$mpg_comb_norm <- ((model.input$mpg_comb_norm * weights.ski[1]))
model.ski$is_crossover <- ((model.input$is_crossover * weights.ski[2]))
model.ski$is_suv <- ((model.input$is_suv * weights.ski[3]))
model.ski$is_wagon  <- ((model.input$is_wagon * weights.ski[4]))
model.ski$is_awd <- ((model.input$is_awd * weights.ski[5]))
model.ski$has_rack <- ((model.input$has_rack * weights.ski[6]))
model.ski$score <- rowSums(model.ski[4:9])

##Search Results
model.ski <- merge(getaround, model.ski, by  = c("cars.make", "cars.model", "cars.year"))
model.ski$score.dist <- (model.ski$distance.factor * model.ski$score)
results.ski <- distinct(rbind((model.ski %>% arrange(desc(score.dist)) %>% slice(1:10)), (model.ski %>% arrange(desc(score)) %>% slice(1:10))))

#Date Night
##rm(model.date)
model.date <- model.input %>%
  select(cars.make, cars.model, cars.year)

##Weight Model Inputs & Score
weights.date <- c(.50, .10, .10, .10, .20 )

model.date$is_luxury <- ((model.input$is_luxury * weights.date[1]))
model.date$has_leather <- ((model.input$has_leather * weights.date[2]))
model.date$has_heated_seat <- ((model.input$has_heated_seat * weights.date[3]))
model.date$has_navi  <- ((model.input$has_navi * weights.date[4]))
model.date$speaker_norm <- ((model.input$speaker_norm * weights.date[5]))
model.date$score <- rowSums(model.date[4:8])

##Search Results
model.date <- merge(getaround, model.date, by  = c("cars.make", "cars.model", "cars.year"))
model.date$score.dist <- (model.date$distance.factor * model.date$score)
results.date <- distinct(rbind((model.date %>% arrange(desc(score.dist)) %>% slice(1:10)), (model.date %>% arrange(desc(score)) %>% slice(1:10))))
