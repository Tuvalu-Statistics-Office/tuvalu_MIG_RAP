##processing arrival and departure

#load libraries
library(readxl) #used to import excel files
library(tidyverse)
library(dplyr)

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Import excel files for both arrivals and departures
arrivals <- read_excel("data/arr_2021_23.xlsx")
departure <- read_excel("data/dep_2021_23.xlsx")

#renaming column names in arrival table
colnames(arrivals)[colnames(arrivals) == "FLIGHT/ SHIP#"] <- "flightship"
colnames(arrivals)[colnames(arrivals) == "TRANSPORT"] <- "transport"
colnames(arrivals)[colnames(arrivals) == "DATE OF ARRIVAL"] <- "dateArrival"
colnames(arrivals)[colnames(arrivals) == "PAX#"] <- "pax"
colnames(arrivals)[colnames(arrivals) == "SURNAME"] <- "lname"
colnames(arrivals)[colnames(arrivals) == "FIRST NAME"] <- "fname"
colnames(arrivals)[colnames(arrivals) == "COUNTRY OF BIRTH"] <- "birthCountry"
colnames(arrivals)[colnames(arrivals) == "DATE OF BIRTH"] <- "dob"
colnames(arrivals)[colnames(arrivals) == "OCCUPATION"] <- "occupation"
colnames(arrivals)[colnames(arrivals) == "COUNTRY OF CITIZENSHIP"] <- "countryCode"
colnames(arrivals)[colnames(arrivals) == "PERMANENT ADDRESS"] <- "permAddress"
colnames(arrivals)[colnames(arrivals) == "SEX"] <- "sex"
colnames(arrivals)[colnames(arrivals) == "PASSPORT#"] <- "passport"
colnames(arrivals)[colnames(arrivals) == "PURPOSE OF VISIT"] <- "purpVisit"
colnames(arrivals)[colnames(arrivals) == "DURATION OF STAY#"] <- "durStay"
colnames(arrivals)[colnames(arrivals) == "OTHER PURPOSE"] <- "othPurpose"

#columns after "othPurpose" will be dropped as they are columns in the data entry worksheet that are 
#dependent on other data in data entry worksheet or from other worksheets.

arrivals <- arrivals[, -which(names(arrivals)=="DURATION OF STAY")]
arrivals <- arrivals[, -which(names(arrivals)=="OTHER PURPOSE (DETAILS)")]
arrivals <- arrivals[, -which(names(arrivals)=="CITIZEN COUNTRY")]
arrivals <- arrivals[, -which(names(arrivals)=="REGION CODE")]
arrivals <- arrivals[, -which(names(arrivals)=="REGION NAME")]
arrivals <- arrivals[, -which(names(arrivals)=="RESIDENT STATUS")]
arrivals <- arrivals[, -which(names(arrivals)=="AGE")]
arrivals <- arrivals[, -which(names(arrivals)=="AGE GROUP","REGION CODE")]
#arrivals <- arrivals[, -which(names(arrivals)=="REGION CODE")]
arrivals <- arrivals[, -which(names(arrivals)=="STAY DURATION GROUP")]
arrivals <- arrivals[, -which(names(arrivals)=="Month")]
arrivals <- arrivals[, -which(names(arrivals)=="MONTH/YEAR")]
arrivals <- arrivals[, -which(names(arrivals)=="Quarters")]
arrivals <- arrivals[, -which(names(arrivals)=="ACTUAL DOB")]
arrivals <- arrivals[, -which(names(arrivals)=="year")]
arrivals <- arrivals[, -which(names(arrivals)=="Expected date of De-arture")]

#renaming columns in departure table
colnames(departure)[colnames(departure) == "FLIGHT/ SHIP#"] <- "flightship"
colnames(departure)[colnames(departure) == "TM"] <- "transport"
colnames(departure)[colnames(departure) == "DESTINATION"] <- "destination"
colnames(departure)[colnames(departure) == "DATE OF DEPARTURE"] <- "dateDeparture"
colnames(departure)[colnames(departure) == "Pax#"] <- "pax"
colnames(departure)[colnames(departure) == "SURNAME"] <- "lname"
colnames(departure)[colnames(departure) == "FIRST NAME"] <- "fname"
colnames(departure)[colnames(departure) == "PASSPORT#"] <- "passport"
colnames(departure)[colnames(departure) == "NATIONALITY"] <- "countryCode"
colnames(departure)[colnames(departure) == "DATE OF BIRTH"] <- "dob"
colnames(departure)[colnames(departure) == "SEX"] <- "sex"
colnames(departure)[colnames(departure) == "RESID_ ENCE"] <- "residence"
colnames(departure)[colnames(departure) == "POT/ POV"] <- "purpTravel"
colnames(departure)[colnames(departure) == "Details of Other Purpose"] <- "otherPurpTravel"
colnames(departure)[colnames(departure) == "RESIDENT (DAYS AWAY)"] <- "daysAway"
colnames(departure)[colnames(departure) == "FLIGHT/SHIP#"] <- "flight_ship"
colnames(departure)[colnames(departure) == "NON_RESIDENT (DATE OF ARRIVAL)"] <- "dateArrival"

#columns after "dateArrival" will be dropped as they are columns in the data entry worksheet that are 
#dependent on other data in data entry worksheet or from other worksheets.
departure <- departure[, -which(names(departure)=="COUNTRY")]
departure <- departure[, -which(names(departure)=="Region Code")]
departure <- departure[, -which(names(departure)=="Region Name")]
departure <- departure[, -which(names(departure)=="AGE")]
departure <- departure[, -which(names(departure)=="AGE GROUP")]
departure <- departure[, -which(names(departure)=="Days Away Groups")]
departure <- departure[, -which(names(departure)=="NON-RESIDENT (DAYS)")]
departure <- departure[, -which(names(departure)=="MONTH")]
departure <- departure[, -which(names(departure)=="MONTH/ YEAR")]
departure <- departure[, -which(names(departure)=="year")]
departure <- departure[, -which(names(departure)=="QTR")]

#-----------------------------------------------------------------------------------
#Processing arrival
#-----------------------------------------------------------------------------------
#Step 1 - Drop rows with missing date of arrival.
#These records are dropped as there is not much that can be done in terms of imputation. Until CSD agrees on how to impute the dates
#or receives assistance on missing dates, not much can be done.
#!!!Note the number of obs before dropping = 2237
arrivals <- arrivals[!is.na(arrivals$dateArrival), ]
#!!!Note the number of obs after dropping = 1783
#!!!Note the difference = 454
#The number of obs before and aftrer dropping will be different for different datasets

#Step 2 - Formating the date of arrival and create column month and year
arrivals$dateArrival = ymd(arrivals$dateArrival)
arrivals$month = month(arrivals$dateArrival)
arrivals$year = year(arrivals$dateArrival)

#Step 3 - Check for years that may be incorrect. Drop record if it cannot be corrected.
curYear <- max(arrivals$year)
arrivals <- arrivals[arrivals$year > curYear-10, ]

#Step 4 - Get country description, region code, and region description
country_region <- read_excel("data/country_region.xlsx") #load country and region file
arrivals <- merge(arrivals, country_region, by = "countryCode", all = TRUE) #merge files
arrivals <- arrivals[!is.na(arrivals$flightship), ] #drop empty rows
arrivals$countryCode[is.na(arrivals$countryCode)] <- "Missing"
arrivals$countryName[is.na(arrivals$countryName)] <- "Missing"
arrivals$regionCode[is.na(arrivals$regionCode)] <- "Missing"
arrivals$regionCode[is.na(arrivals$regionCode)] <- "Missing"

#Step 5 - Generate resident status
#!!Need help in using if statements
arrivals$resident <- ifelse(arrivals$purpVisit==1,1,2)

#Step 6 - Calculate age
arrivals$dobYear <- year(arrivals$dob)
arrivals$age <- arrivals$year - arrivals$dobYear
arrivals$age[is.na(arrivals$age)] <- "Missing"

#Step 7 - Get age groups
age_group <- read_excel("data/ageGroup.xlsx")
arrivals <- merge(arrivals, age_group, by = "age", all = TRUE)