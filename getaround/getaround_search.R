#Install and Load required packages
##install.packages("dplyr")
##install.packages("tidyjson")
##install.packages("jsonlite")
##install.packages("tidyr")
library("dplyr")
library("tidyjson")
library("jsonlite")
library("tidyr")

#rm(list=ls(all=TRUE))

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


#Transform data for models
##Rename columns
colnames(model.data)[17] <- "zero_sixty"

##Reformat data types
model.data$zero_sixty <- as.numeric(as.character(model.data$zero_sixty))
model.data$power_weight <- (as.numeric(model.data$engine.horsepower) / as.numeric(model.data$`Curb Weight`))


#Normalize data
model.fun.output <- model.data %>%
  select(styles.id)

###Use scale function to normalize
#!Update this to look for all columns at once and perform the function
model.fun.output$zero_sixy.scale.scale <- scale(model.data$zero_sixty, center = FALSE, scale = TRUE)
model.fun.output$power_weight <- scale(model.data$power_weight, center = FALSE, scale = TRUE)


##Vehicle categorial attributes into binary
model.fun.output$market <- as.numeric(grepl("Performance", model.data$categories.market)|grepl("Exotic", model.data$categories.market)|grepl("Factory Tuner",model.data$categories.market))
model.fun.output$vehiclestyle <- as.numeric(grepl("Coupe", model.data$categories.vehiclestyle)|grepl("Convertible", model.data$categories.vehiclestyle))
model.fun.output$drivewheels <- as.numeric(grepl("all wheel drive", model.data$styles.drivewheels)|grepl("rear wheel drive", model.data$styles.drivewheels))


#Scale output using weights
fun.weights <- c(0.50, 0.20, 0.10, 0.10, 0.10)
model.fun.output$zero_sixty_scaled <- 1 - ((model.fun.output$zero_sixy.scale.scale * fun.weights[1]))
model.fun.output$power_weight_scaled <- ((model.fun.output$power_weight * fun.weights[2]))
model.fun.output$market_scaled <- ((model.fun.output$market * fun.weights[3]))
model.fun.output$vehiclestyle_scaled <- ((model.fun.output$vehiclestyle * fun.weights[4]))
model.fun.output$drivewheels_scaled <- ((model.fun.output$drivewheels * fun.weights[5]))



#Sum Score
##model.fun.output$score <- (model.fun.output$zero_sixty + model.fun.output$power_weight + model.fun.output$market + model.fun.output$vehiclestyle + model.fun.output$drivewheels)
model.fun.output$score <- (model.fun.output$zero_sixty_scaled + model.fun.output$power_weight + model.fun.output$market_scaled + model.fun.output$vehiclestyle_scaled + model.fun.output$drivewheels_scaled)
model.data$score.fun2 <- model.fun.output$score
