#Install and Load required packages
##install.packages("dplyr")
##install.packages("tidyjson")
##install.packages("jsonlite")
##install.packages("tidyr")
library("dplyr")
library("tidyjson")
library("jsonlite")
library("tidyr")

#rm(list=ls(all=TRUE)) rm(model.data)

#Set working directory
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/data_science/getaround")


#1.Load Raw Data
#Load csv files
getaround <- read.csv("getaround_clean_20170116.csv", stringsAsFactors=FALSE)
edmunds_style <- read.csv("edmunds_styles_orig_20170116.csv", stringsAsFactors=FALSE)
edmunds_basic <- read.csv("edmunds_styles_basic_orig_20170116.csv", stringsAsFactors=FALSE)
edmunds_detail <- read.csv("edmunds_styles_details_orig_20170116.csv", stringsAsFactors=FALSE)
#edmunds_detail_all <- read.csv("edmunds_styles_details_orig_20170116.csv", stringsAsFactors=FALSE) #Remove later


#Gather data needed for models
edmunds_style <- edmunds_style %>%
  select(styles.id, cars.make, cars.model, cars.year, styles.submodel.fuel, styles.submodel.tuner)  

edmunds_basic <- edmunds_basic %>%
  select(styles.id, styles.drivewheels, styles.doors, categories.market, categories.epaclass, categories.vehiclestyle, engine.horsepower)

model.data <- merge(edmunds_style, edmunds_basic) 

#filter(expr, cell_type %in% c("bj fibroblast", "hesc"))
edmunds_detail <- edmunds_detail %>%
  select(styles.id, attributes.name, attributes.value) %>%
  filter(attributes.name == 'Epa City Mpg' | attributes.name == 'Epa Combined Mpg' | 
          attributes.name == 'Manufacturer 0 60mph Acceleration Time (seconds)' | attributes.name == 'Curb Weight' | 
          attributes.name == 'Overall Length' | attributes.name == 'Wheelbase' | attributes.name == 'Turning Diameter' | 
          attributes.name == 'Epa Interior Volume' | attributes.name == 'Max Cargo Capacity' | attributes.name == 'Cargo Capacity, Rear Seat Down Or Removed' |
          attributes.name == 'Total Number Of Speakers' | attributes.name == 'Watts' | attributes.name == 'Cd Player' | attributes.name == 'Cd Mp3 Playback' | attributes.name == 'Usb Connection' | attributes.name == 'Digital Audio Input' | 
            attributes.name == 'Memory Card Slot' | attributes.name == 'Hard Drive' | attributes.name == 'Premium Brand Audio System' | 
            attributes.name == 'Premium Brand Speakers'| attributes.name == 'Subwoofer'| attributes.name == 'Surround Audio'| attributes.name == 'Radio Data System'| attributes.name == 'Satellite Radio'| attributes.name == 'Audio Controls On Steering Wheel'| 
            attributes.name == 'Speed Sensitive Volume Control' | 
           attributes.name == 'Roof Rack' | 
           attributes.name == '1st Row Seating Capacity' | attributes.name == '2nd Row Seating Capacity' | attributes.name == '3rd Row Seating Capacity' |
           attributes.name == '1st Row Upholstery') %>%
  spread(attributes.name, attributes.value)

model.data <-  merge(model.data, edmunds_detail)


#2.Cleanse Data for Models
##Rename columns
#colnames(model.data)[23] <- "zero_sixty"

##Reformat data types
##? Can I do this in all one shot?
#model.data$power_weight <- (as.numeric(model.data$engine.horsepower) / as.numeric(model.data$`Curb Weight`))
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
#!!Impute data points...revisit original dataset
model.data$zero_sixty[is.na(model.data$zero_sixty)] <- mean(model.data$zero_sixty, na.rm = TRUE)


##Getaround Distances
getaround$distance.factor[getaround$cars.distance <= .25] <- 1.00 #Local
getaround$distance.factor[getaround$cars.distance > .25 & getaround$cars.distance <= 1] <- .80 #Walkable
getaround$distance.factor[getaround$cars.distance > 1 & getaround$cars.distance <= 2]  <- .60 #Bikeable
getaround$distance.factor[getaround$cars.distance > 2 & getaround$cars.distance <= 5]  <- .40 #Uberable
getaround$distance.factor[getaround$cars.distance > 5] <- .20 #Bartable

#########################################################################################################


#Build models
#Fun to Drive
#rm(model.fun.output)
model.fun.output <- model.data %>%
  select(styles.id, cars.make, cars.model, cars.year)
#??Remove trucks SUVs


##Use scale function to normalize
model.fun.output$zero_sixty_norm <- 1 - (scale(model.data$zero_sixty, center = min(model.data$zero_sixty, na.rm = TRUE), scale = diff(range(model.data$zero_sixty, na.rm = TRUE))))

##Vehicle categorial attributes into binary
###Vehicle Market
model.fun.output$is_performance <- as.numeric(grepl('Performance', model.data$categories.market))
model.fun.output$is_tuner <- as.numeric(grepl('Factory Tuner', model.data$categories.market))
model.fun.output$is_exotic <- as.numeric(grepl('Exotic', model.data$categories.market))

###Vehicle Style
model.fun.output$is_coupe <- as.numeric(grepl('Coupe', model.data$categories.vehiclestyle))
model.fun.output$is_convertible <- as.numeric(grepl('Convertible', model.data$categories.vehiclestyle))
model.fun.output$is_hatchback <- as.numeric(grepl('2dr Hatchback', model.data$categories.vehiclestyle))

###Drive Wheels
model.fun.output$is_awd <- as.numeric(grepl('all wheel drive', model.data$styles.drivewheels))
model.fun.output$is_rwd <- as.numeric(grepl('rear wheel drive', model.data$styles.drivewheels))

##Scale output using weights
weights.fun <- c(.40, .04,.16,.10, .06,.10,.04, .02,.08)

model.fun.output$zero_sixty_norm <- (model.fun.output$zero_sixty_norm * weights.fun[1])
model.fun.output$is_performance <- (model.fun.output$is_performance * weights.fun[2])
model.fun.output$is_tuner <- (model.fun.output$is_tuner * weights.fun[3])
model.fun.output$is_exotic <- (model.fun.output$is_exotic * weights.fun[4])
model.fun.output$is_coupe <- (model.fun.output$is_coupe * weights.fun[5])
model.fun.output$is_convertible <- (model.fun.output$is_convertible * weights.fun[6])
model.fun.output$is_hatchback <- (model.fun.output$is_hatchback * weights.fun[7])
model.fun.output$is_awd <- (model.fun.output$is_awd * weights.fun[8])
model.fun.output$is_rwd <- (model.fun.output$is_rwd * weights.fun[9])

##Sum Score
model.fun.output$score <- rowSums(model.fun.output[5:13])

##Getaround search results
#rm(getaround.fun)
getaround.fun <- merge(getaround, model.fun.output, by  = c("cars.make","cars.model","cars.year"))
getaround.fun$score.distance <- (getaround.fun$distance.factor * getaround.fun$score)


#############
#Run Errands
#rm(model.errands.output)
model.errands.output <- model.data %>%
  select(styles.id, cars.make, cars.model, cars.year)
#Remove SUVS and trucks

##Use scale function to normalize
model.errands.output$mpg_city_norm <- (scale(model.data$'Epa City Mpg', center = min(model.data$'Epa City Mpg', na.rm = TRUE), scale = diff(range(model.data$'Epa City Mpg', na.rm = TRUE))))
model.errands.output$turning_norm <- 1 - (scale(model.data$'Turning Diameter', center = min(model.data$'Turning Diameter', na.rm = TRUE), scale = diff(range(model.data$'Turning Diameter', na.rm = TRUE))))
model.errands.output$length_norm <- 1 - (scale(model.data$'Overall Length', center = min(model.data$'Overall Length', na.rm = TRUE), scale = diff(range(model.data$'Overall Length', na.rm = TRUE))))
model.errands.output$cargo_norm <- (scale(model.data$'Max Cargo Capacity', center = min(model.data$'Max Cargo Capacity', na.rm = TRUE), scale = diff(range(model.data$'Max Cargo Capacity', na.rm = TRUE))))

##Vehicle categorial attributes into binary
#model.errands.output$ <- as.numeric(grepl("Performance", model.data$categories.market)|grepl("Exotic", model.data$categories.market)|grepl("Factory Tuner",model.data$categories.market))

##Scale output using weights
weights.errands <- c(.30, .20, .20, .30)

model.errands.output$mpg_city_norm <- ((model.errands.output$mpg_city_norm * weights.errands[1]))
model.errands.output$turning_norm <- ((model.errands.output$turning_norm * weights.errands[2]))
model.errands.output$length_norm <- ((model.errands.output$length_norm * weights.errands[3]))
model.errands.output$cargo_norm <- ((model.errands.output$cargo_norm * weights.errands[4]))

##Sum Score
model.errands.output$score <- rowSums(model.errands.output[5:8])

##Getaround search results
getaround.errands <- merge(getaround, model.errands.output, by  = c("cars.make","cars.model","cars.year"))
getaround.errands$score.distance <- (getaround.errands$distance.factor * getaround.errands$score)


#Audiophile#
#rm(model.audio.output)
model.audio.output <- model.data %>%
  select(styles.id, cars.make, cars.model, cars.year)

##Use scale function to normalize
#model.audio.output$speaker_norm <- scale(model.data$'Total Number Of Speakers', center = FALSE, scale = TRUE)
model.audio.output$speaker_norm <- (scale(model.data$'Total Number Of Speakers', center = min(model.data$'Total Number Of Speakers', na.rm = TRUE), scale = diff(range(model.data$'Total Number Of Speakers', na.rm = TRUE))))


##Vehicle categorial attributes into binary
model.audio.output$has_cd <- as.numeric(grepl('*', model.data$'Cd Player'))
model.audio.output$has_mp3 <- as.numeric(grepl('*', model.data$'Cd Mp3 Playback'))
model.audio.output$has_usb <- as.numeric(grepl('*', model.data$'Usb Connection'))
model.audio.output$has_digital_input <- as.numeric(grepl('*', model.data$'Digital Audio Input'))
model.audio.output$has_mem_card <- as.numeric(grepl('*', model.data$'Memory Card Slot'))
model.audio.output$has_hd <- as.numeric(grepl('*', model.data$'Hard Drive'))
model.audio.output$has_prem_audio <- as.numeric(grepl('*', model.data$'Premium Brand Audio System'))
model.audio.output$has_prem_speaker <- as.numeric(grepl('*', model.data$'Premium Brand Speakers'))
model.audio.output$has_sub <- as.numeric(grepl('*', model.data$'Subwoofer'))
model.audio.output$has_surround <- as.numeric(grepl('*', model.data$'Surround Audio'))
model.audio.output$has_rds <- as.numeric(grepl('*', model.data$'Radio Data System'))
model.audio.output$has_sat <- as.numeric(grepl('*', model.data$'Satellite Radio'))
model.audio.output$has_steering <- as.numeric(grepl('*', model.data$'Audio Controls On Steering Wheel'))
model.audio.output$has_speed_vol <- as.numeric(grepl('*', model.data$'Speed Sensitive Volume Control'))

##Scale output using weights
weights.audiophile <- c(0.35, 0.025, 0.05,  0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.025)
model.audio.output$speaker_norm <- ((model.audio.output$speaker_norm * weights.audiophile[1]))
model.audio.output$has_cd <- ((model.audio.output$has_cd * weights.audiophile[2]))
model.audio.output$has_mp3 <- ((model.audio.output$has_mp3 * weights.audiophile[3]))
model.audio.output$has_usb <- ((model.audio.output$has_usb * weights.audiophile[4]))
model.audio.output$has_digital_input <-  ((model.audio.output$has_digital_input * weights.audiophile[5]))
model.audio.output$has_mem_card <-  ((model.audio.output$has_mem_card * weights.audiophile[6]))
model.audio.output$has_hd <-  ((model.audio.output$has_hd  * weights.audiophile[7]))
model.audio.output$has_prem_audio <-  ((model.audio.output$has_prem_audio * weights.audiophile[8]))
model.audio.output$has_prem_speaker <-  ((model.audio.output$has_prem_speaker * weights.audiophile[9]))
model.audio.output$has_sub <-  ((model.audio.output$has_sub * weights.audiophile[10]))
model.audio.output$has_surround <-  ((model.audio.output$has_surround * weights.audiophile[11]))
model.audio.output$has_rds <-  ((model.audio.output$has_rds * weights.audiophile[12]))
model.audio.output$has_sat <-  ((model.audio.output$has_sat * weights.audiophile[13]))
model.audio.output$has_steering <-  ((model.audio.output$has_steering * weights.audiophile[14]))
model.audio.output$has_speed_vol <-  ((model.audio.output$has_speed_vol * weights.audiophile[15]))

##!!Figure out how to multiply vector by data frame
#model.audio.output.test <- (model.audio.output[5:15] * weights.audiophile)
#res <- lapply(model.audio.output[5:15], "*", weights.audiophile)
#res <- as.data.frame(res)
#weights.audiophile <- as.matrix(weights.audiophile)

##Sum Score
model.audio.output$score <- rowSums(model.audio.output[5:19])

##Getaround search results
getaround.audio <- merge(getaround, model.audio.output, by  = c("cars.make","cars.model","cars.year"))
getaround.audio$score.distance <- (getaround.audio$distance.factor * getaround.audio$score)


#Ski Trip
#rm(model.ski.output)
model.ski.output <- model.data %>%
  select(styles.id, cars.make, cars.model, cars.year)

##Use scale function to normalize
model.ski.output$mpg_comb_norm <- (scale(model.data$'Epa Combined Mpg', center = min(model.data$'Epa Combined Mpg', na.rm = TRUE), scale = diff(range(model.data$'Epa Combined Mpg', na.rm = TRUE))))

##Vehicle categorial attributes into binary
model.ski.output$is_crossover <- as.numeric(grepl('Crossover', model.data$categories.market))
model.ski.output$is_suv <- as.numeric(grepl('SUV', model.data$categories.vehiclestyle))
model.ski.output$is_wagon <- as.numeric(grepl('Wagon', model.data$categories.vehiclestyle))
model.ski.output$is_awd <- as.numeric(grepl('all wheel drive', model.data$styles.drivewheels))
model.ski.output$is_4wd <- as.numeric(grepl('four wheel drive', model.data$styles.drivewheels))
model.ski.output$has_rack <- as.numeric(grepl('*', model.data$'Roof Rack'))

##Scale output using weights
#rm(model.ski.output)
weights.ski <- c(.15, .05, .20, .20, .10, .10, .20)

model.ski.output$mpg_comb_norm <- ((model.ski.output$mpg_comb_norm * weights.ski[1]))
model.ski.output$is_crossover <- ((model.ski.output$is_crossover * weights.ski[2]))
model.ski.output$is_suv <- ((model.ski.output$is_suv * weights.ski[3]))
model.ski.output$is_wagon  <- ((model.ski.output$is_wagon * weights.ski[4]))
model.ski.output$is_awd <- ((model.ski.output$is_awd * weights.ski[5]))
model.ski.output$is_4wd <- ((model.ski.output$is_4wd * weights.ski[6]))
model.ski.output$has_rack <- ((model.ski.output$has_rack * weights.ski[7]))

##Sum Score
model.ski.output$score <- rowSums(model.ski.output[5:11])

##Getaround search results
getaround.ski <- merge(getaround, model.ski.output, by  = c("cars.make","cars.model","cars.year"))
getaround.ski$score.distance <- (getaround.ski$distance.factor * getaround.ski$score)

model.audio.output$score <- rowSums(model.audio.output[5:15])