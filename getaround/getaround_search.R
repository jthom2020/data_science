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

#Gather data needed for models
edmunds_style <- edmunds_style %>%
  select(styles.id, cars.make, cars.model, cars.year, styles.submodel.fuel, styles.submodel.tuner)  

edmunds_basic <- edmunds_basic %>%
  select(styles.id, styles.drivewheels, styles.doors, categories.market, categories.epaclass, categories.vehiclestyle, engine.horsepower)

model.data <- merge(edmunds_style, edmunds_basic) 

edmunds_detail <- edmunds_detail %>%
  select(styles.id, attributes.name, attributes.value) %>%
  filter(attributes.name == 'Epa City Mpg' | attributes.name == 'Epa Combined Mpg' | 
          attributes.name == 'Manufacturer 0 60mph Acceleration Time (seconds)' | attributes.name == 'Curb Weight' | 
          attributes.name == 'Overall Length' | attributes.name == 'Wheelbase' | attributes.name == 'Turning Diameter' | 
          attributes.name == 'Epa Interior Volume' | attributes.name == 'Max Cargo Capacity' | attributes.name == 'Cargo Capacity, Rear Seat Down Or Removed' |
          attributes.name == 'Total Number Of Speakers' | attributes.name == 'Watts' | attributes.name == 'Cd Player' | attributes.name == 'Cd Mp3 Playback' | attributes.name == 'Usb Connection' | attributes.name == 'Digital Audio Input' | 
            attributes.name == 'Memory Card Slot' | attributes.name == 'Hard Drive' | attributes.name == 'Premium Brand Audio System' | 
            attributes.name == 'Premium Brand Speakers'| attributes.name == 'Subwoofer'| attributes.name == 'Surround Audio'| attributes.name == 'Radio Data System'| attributes.name == 'Satellite Radio'| attributes.name == 'Audio Controls On Steering Wheel'| 
            attributes.name == 'Speed Sensitive Volume Control') %>%
  spread(attributes.name, attributes.value)

model.data <-  merge(model.data, edmunds_detail)


#2.Cleanse Data for Models
##Rename columns
colnames(model.data)[23] <- "zero_sixty"

##Reformat data types
##? Can I do this in all one shot?
#model.data$power_weight <- (as.numeric(model.data$engine.horsepower) / as.numeric(model.data$`Curb Weight`))
model.data$zero_sixty <- as.numeric(model.data$zero_sixty)
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

##Fill in missing data points
#!!Impute data points...revisit original dataset
model.data$zero_sixty[is.na(model.data$zero_sixty)] <- mean(model.data$zero_sixty, na.rm = TRUE)

#####################
#############
#Build models
#Fun to Drive
model.fun.output <- model.data %>%
  select(styles.id, cars.make, cars.model, cars.year)

##Use scale function to normalize
model.fun.output$zero_sixty_norm <- scale(model.data$zero_sixty, center = FALSE, scale = TRUE)
model.fun.output$power_weight <- scale(model.data$power_weight, center = FALSE, scale = TRUE)

##Vehicle categorial attributes into binary
model.fun.output$market <- as.numeric(grepl("Performance", model.data$categories.market)|grepl("Exotic", model.data$categories.market)|grepl("Factory Tuner",model.data$categories.market))
model.fun.output$vehiclestyle <- as.numeric(grepl("Coupe", model.data$categories.vehiclestyle)|grepl("Convertible", model.data$categories.vehiclestyle))
model.fun.output$drivewheels <- as.numeric(grepl("all wheel drive", model.data$styles.drivewheels)|grepl("rear wheel drive", model.data$styles.drivewheels))

##Scale output using weights
fun.weights <- c(0.65, 0.00, 0.05, 0.25, 0.05)
model.fun.output$zero_sixty_scaled <- 1 - ((model.fun.output$zero_sixty_norm * fun.weights[1]))
model.fun.output$power_weight_scaled <- ((model.fun.output$power_weight * fun.weights[2]))
model.fun.output$market_scaled <- ((model.fun.output$market * fun.weights[3]))
model.fun.output$vehiclestyle_scaled <- ((model.fun.output$vehiclestyle * fun.weights[4]))
model.fun.output$drivewheels_scaled <- ((model.fun.output$drivewheels * fun.weights[5]))

##Sum Score
model.fun.output$score <- (model.fun.output$zero_sixty_scaled + model.fun.output$market_scaled + model.fun.output$vehiclestyle_scaled + model.fun.output$drivewheels_scaled)# + model.fun.output$power_weight

##Getaround search results
model.fun.output <- merge(getaround, model.fun.output)

#Run Errands
model.errands.output <- model.data %>%
  select(styles.id, cars.make, cars.model, cars.year)

##Use scale function to normalize
model.errands.output$mpg_city_norm <- scale(model.data$'Epa City Mpg', center = FALSE, scale = TRUE)
model.errands.output$turning_norm <- scale(model.data$'Turning Diameter', center = FALSE, scale = TRUE)

##Vehicle categorial attributes into binary
model.fun.output$market <- as.numeric(grepl("Performance", model.data$categories.market)|grepl("Exotic", model.data$categories.market)|grepl("Factory Tuner",model.data$categories.market))
model.fun.output$vehiclestyle <- as.numeric(grepl("Coupe", model.data$categories.vehiclestyle)|grepl("Convertible", model.data$categories.vehiclestyle))
model.fun.output$drivewheels <- as.numeric(grepl("all wheel drive", model.data$styles.drivewheels)|grepl("rear wheel drive", model.data$styles.drivewheels))

##Scale output using weights
errands.weights <- c(0.65, 0.00, 0.05, 0.25, 0.05)
model.fun.output$zero_sixty_scaled <- ((model.fun.output$zero_sixty_norm * fun.weights[1]))
model.fun.output$power_weight_scaled <- ((model.fun.output$power_weight * fun.weights[2]))
model.fun.output$market_scaled <- ((model.fun.output$market * fun.weights[3]))
model.fun.output$vehiclestyle_scaled <- ((model.fun.output$vehiclestyle * fun.weights[4]))
model.fun.output$drivewheels_scaled <- ((model.fun.output$drivewheels * fun.weights[5]))

##Sum Score
model.errands.output$score <- (model.fun.output$zero_sixty_scaled + model.fun.output$market_scaled + model.fun.output$vehiclestyle_scaled + model.fun.output$drivewheels_scaled)# + model.fun.output$power_weight


#Build models
#Audiophile
model.audio.output <- model.data %>%
  select(styles.id, cars.make, cars.model, cars.year)

##Use scale function to normalize
model.audio.output$speaker_norm <- scale(model.data$'Total Number Of Speakers', center = FALSE, scale = TRUE)
model.audio.output$watts_norm <- scale(model.data$'Watts', center = FALSE, scale = TRUE)

##Vehicle categorial attributes into binary
model.audio.output$has_cd <- as.numeric(grepl('*', model.data$'Cd Player'))


##Scale output using weights
errands.weights <- c(0.65, 0.00, 0.05, 0.25, 0.05)
model.fun.output$zero_sixty_scaled <- ((model.fun.output$zero_sixty_norm * fun.weights[1]))
model.fun.output$power_weight_scaled <- ((model.fun.output$power_weight * fun.weights[2]))
model.fun.output$market_scaled <- ((model.fun.output$market * fun.weights[3]))
model.fun.output$vehiclestyle_scaled <- ((model.fun.output$vehiclestyle * fun.weights[4]))
model.fun.output$drivewheels_scaled <- ((model.fun.output$drivewheels * fun.weights[5]))

##Sum Score
model.errands.output$score <- (model.fun.output$zero_sixty_scaled + model.fun.output$market_scaled + model.fun.output$vehiclestyle_scaled + model.fun.output$drivewheels_scaled)# + model.fun.output$power_weight
