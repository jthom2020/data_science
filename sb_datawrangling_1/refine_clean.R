#Load libraries
library(stringr)


#0:Load the data in RStudio
refine_df <- read.csv("/Users/jonathonthompson/Documents/data_science/refine_original.csv")


#1:Clean up brand names
refine_df$company <- tolower(refine_df$company)
refine_df$company[grepl("^ak", refine_df$company)] <- "akzo"
refine_df$company[grepl("ps$", refine_df$company)] <- "phillips"
refine_df$company[grepl("^uni", refine_df$company)] <- "unilever"


#2:Separate product code and number
refine_df <- separate(refine_df, col = Product.code...number, c("product_code", "product_number"))


#3:Add product categories
refine_df$product_category <- refine_df$product_code
refine_df$product_category <- str_replace(refine_df$product_category, "p", "Smartphone")
refine_df$product_category <- str_replace(refine_df$product_category, "v", "TV")
refine_df$product_category <- str_replace(refine_df$product_category, "x", "Laptop")
refine_df$product_category <- str_replace(refine_df$product_category, "q", "Tablet")


#4:Add full address for geocoding
refine_df$full_address <- paste(refine_df$address, refine_df$city, refine_df$country, sep = ", ") 


#5:Create dummy variables for company and product category
company_akzo <- as.numeric(refine_df$company == "akzo")
company_phillips <- as.numeric(refine_df$company == "phillips")
company_unilever <- as.numeric(refine_df$company == "unilever")
company_van_houten <- as.numeric(refine_df$company == "van houten")

refine_df <- cbind(refine_df, company_akzo, company_phillips, company_unilever, company_van_houten)

product_laptop <- as.numeric(refine_df$product_category == "Laptop")
product_smartphone <- as.numeric(refine_df$product_category == "Smartphone")
product_tablet <- as.numeric(refine_df$product_category == "Tablet")
product_tv <- as.numeric(refine_df$product_category == "TV")

refine_df <- cbind(refine_df, product_laptop, product_smartphone, product_tablet, product_tv)

#Write to csv
write.csv(refine_df, "refine_clean.csv" )
