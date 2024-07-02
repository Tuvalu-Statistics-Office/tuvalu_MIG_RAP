#Testing Push and Pull
#Lae Peleti
#Senior Statistician
#Central Statistics Division
#Ministry of Finance and Economic Development
#Government of Tuvalu

##processing arrival and departure

#load libraries
library(readxl) #used to import excel files
library(tidyverse)
library(dplyr)
library(RSQLite)
library(pivottabler)

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Import excel files for both arrivals and departures
arrivals <- read_excel("data/arrivals.xlsx")

#Connect to db
mydb <- dbConnect(RSQLite::SQLite(), "data/migration.db")

#----------------------------------------------------------------------------------------------------------------------
#1 Processing arrival data
#----------------------------------------------------------------------------------------------------------------------

#Step 1.0 - Renaming column names in arrival table
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
colnames(arrivals)[colnames(arrivals) == "DURATION OF STAY"] <- "durStay"
colnames(arrivals)[colnames(arrivals) == "OTHER PURPOSE"] <- "othPurpose"

#columns after "othPurpose" will be dropped as they are columns in the data entry worksheet that are 
#dependent on other data in data entry worksheet or from other worksheets.

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

#Step 1.1 - Drop rows with missing date of arrival.
#These records are dropped as there is not much that can be done in terms of imputation. Until CSD agrees on how to impute the dates
#or receives assistance on missing dates, not much can be done.
#!!!Note the number of obs before dropping = 10,296
arrivals <- arrivals[!is.na(arrivals$dateArrival), ]
#!!!Note the number of obs after dropping = 9,882
#!!!Note the difference = 414
#The number of obs before and after dropping will be different for different data sets

#Step 1.2 - Formatting the date of arrival and create column month and year
arrivals$dateArrival = ymd(arrivals$dateArrival)
arrivals$month = month(arrivals$dateArrival)
arrivals$year = year(arrivals$dateArrival)
#!!!Note: ASO should always check the formats of the dates.

#Step 1.3 - Check for years that may be incorrect. Drop record if it cannot be corrected.
curYearA <- max(arrivals$year)
arrivals$curYearA <- curYearA
arrivals <- arrivals[arrivals$year > curYearA-10, ]
#!!!Note: Dropping is a result of incorrect dates. ASO should check the dates.

#Step 1.4 - Get country description, region code, and region description
country_region <- read_excel("data/country_region.xlsx") #load country and region file
arrivals <- merge(arrivals, country_region, by = "countryCode", all = TRUE) #merge files
arrivals <- arrivals[!is.na(arrivals$flightship), ] #drop empty rows
arrivals$countryCode[is.na(arrivals$countryCode)] <- "Missing"
arrivals$countryName[is.na(arrivals$countryName)] <- "Missing"
arrivals$regionCode[is.na(arrivals$regionCode)] <- "Missing"
arrivals$regionCode[is.na(arrivals$regionCode)] <- "Missing"
#!!!Note: Missing country codes will be assigned "Missing". ASO to check that country matches with Passport
#particularly people with Tuvaluan passports should be Tuvaluan nationals.

#Step 1.5 - Generate resident status
arrivals$resident <- ifelse(arrivals$countryCode==3609,1,2)
arrivals$resident <- ifelse(arrivals$purpVisit==1,1,2)
#!!!Note: ASO to check residency. Tuvalu nationals should be residents. There are cases when this is not true.
# Classifying such cases as non-resident should be carefully considered.

#Step 1.6 - Calculate age
arrivals$dobYear <- year(arrivals$dob)
arrivals$age <- arrivals$year - arrivals$dobYear
arrivals$age[is.na(arrivals$age)] <- "Missing"
#!!!Note: Age is only missing if either date of arrival or date of birth is missing. In all cases, these variables
# should not be missing.

#Step 1.7 - Get age groups
age_group <- read_excel("data/ageGroup.xlsx")
arrivals <- merge(arrivals, age_group, by = "age", all = TRUE)
#!!!Note: Age group is only missing if age is missing.

#Step 1.8 - Process duration of stay, a lot of cleaning in required to do at data entry level
#Need to research how to use nested if statements. Use merge for now.
#Calculate duration of stay
arrivals$dateDep <- ymd(arrivals$durStay)
arrivals$durStayCalc <- arrivals$dateDep - arrivals$dateArrival
durStay_Group <- read_excel("data/durStay_Group.xlsx")
arrivals <- merge(arrivals, durStay_Group, by = "durStayCalc", all = TRUE) #merge files
arrivals$stayAwayGroup[is.na(arrivals$stayAwayGroup)] <- "Missing"
arrivals <- arrivals[!is.na(arrivals$flightship), ] #drop empty rows
#!!!Note: There are cases where dates are entered as days away. ASO to note that this variable requires
# the number of days away. ASO to calculate this number if a date is entered in the forms.

#There is a lot to be done to improve the data particularly during the data entry phase.
#See notes for each step.

#Change values for tables
#Residence
arrivals$resident <- ifelse(arrivals$resident==1,"Resident","Visitor")
#Sex
arrivals$sex <- ifelse(arrivals$sex==1,"Male","Female")
#Months
monthtab <- read_excel("data/month.xlsx")
arrivals <- merge(arrivals, monthtab, by = "month", all = TRUE) #merge files
arrivals <- arrivals[!is.na(arrivals$flightship), ] #drop empty rows
#TM
arrivals$transport <- ifelse(arrivals$transport==1,"Air","Sea")
#Purpose of visit
purpVisit <- read_excel("data/purpVisit.xlsx")
arrivals <- merge(arrivals, purpVisit, by = "purpVisit", all = TRUE) #merge files
arrivals <- arrivals[!is.na(arrivals$flightship), ] #drop empty rows
#Step 1.9 - Write arrivals to db
arrivals$N <- 1
dbWriteTable(mydb, "arrivals", arrivals, overwrite = TRUE)

#----------------------------------------------------------------------------------------------------------------------
#2 Processing departure
#----------------------------------------------------------------------------------------------------------------------

#Step 2.0 - Renaming columns in departure table
departure <- read_excel("data/departures.xlsx")
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
colnames(departure)[colnames(departure) == "RESID_ ENCE"] <- "resident"
colnames(departure)[colnames(departure) == "POT/ POV"] <- "purpTravel"
colnames(departure)[colnames(departure) == "Details of Other Purpose"] <- "otherPurpTravel"
colnames(departure)[colnames(departure) == "RESIDENT (DAYS AWAY)"] <- "daysAway"
colnames(departure)[colnames(departure) == "FLIGHT/SHIP#"] <- "flight_ship"
colnames(departure)[colnames(departure) == "NON_RESIDENT (DATE OF ARRIVAL)"] <- "dateArrival"

#columns after "dateDeparture" will be dropped as they are columns in the data entry worksheet that are 
#dependent on other variables in data entry worksheet or from other worksheets.
departure <- departure[, -which(names(departure)=="COUNTRY")]
departure <- departure[, -which(names(departure)=="Region Code")]
departure <- departure[, -which(names(departure)=="Region Name")]
departure <- departure[, -which(names(departure)=="AGE")]
departure <- departure[, -which(names(departure)=="AGE GROUP")]
departure <- departure[, -which(names(departure)=="Days Away Groups")]
#departure <- departure[, -which(names(departure)=="NON_RESIDENT (DATE OF ARRIVAL)")]
departure <- departure[, -which(names(departure)=="MONTH")]
departure <- departure[, -which(names(departure)=="MONTH/ YEAR")]
departure <- departure[, -which(names(departure)=="year")]
departure <- departure[, -which(names(departure)=="QTR")]

#Step 2.1 - Drop rows with missing date of departure.
#These records are dropped as there is not much that can be done in terms of imputation. Until CSD agrees on how to impute the dates
#or receives assistance on missing dates, not much can be done.
#!!!Note the number of obs before dropping = 12,391
departure <- departure[!is.na(departure$dateDeparture), ]
#uncomment the line above to drop entries with missing depature dates
#!!!Note the number of obs after dropping = 12,291
#!!!Note the difference = 100
#The number of obs before and after dropping will be vary for different data sets

#Step 2.2 - Formatting the date of departure and create column month and year
departure$dateDeparture = ymd(departure$dateDeparture)
departure$month = month(departure$dateDeparture)
departure$year = year(departure$dateDeparture)
#!!!Note: ASO should always check the formats of the dates.

#Step 2.3 - Check for years that may be incorrect. Drop record if it cannot be corrected.
curYearD <- max(departure$year)
departure$curYearD <- curYearD
#departure <- departure[departure$year > curYear-10, ]
#!!!Note: Dropping is a result of incorrect dates. ASO should check the dates.

#Step 2.4 - Get country description, region code, and region description
departure <- merge(departure, country_region, by = "countryCode", all = TRUE) #merge files
#departure <- departure[!is.na(departure$flightship), ] #drop empty rows
departure$countryCode[is.na(departure$countryCode)] <- "Missing"
departure$countryName[is.na(departure$countryName)] <- "Missing"
departure$regionCode[is.na(departure$regionCode)] <- "Missing"
departure$regionCode[is.na(departure$regionCode)] <- "Missing"
#!!!Note: Missing country codes will be assigned "Missing". ASO to check that country matches with Passport
#particularly people with Tuvaluan passports should be Tuvaluan nationals.

#Step 2.5 - Checking resident status, note that resident status in departure is not a dependent variable.
departure$resident <- ifelse(departure$countryCode==3609,1,2)
#departure$residence <- [is.na(departure$residence)] <- "Missing"
#!!!Comment previous line if there are no blank residence "Missing".
#!!!Note: ASO to check residency. Tuvalu nationals should be residents. There are cases when this is not true.
# Classifying such cases as non-resident should be carefully considered.

#Step 2.6 - Calculate age
departure$dobYear <- year(departure$dob)
departure$age <- departure$year - departure$dobYear
departure$age[is.na(departure$age)] <- "Missing"
#!!!Note: Age is only missing if either date of arrival or date of birth is missing. In all cases, these variables
# should not be missing.

#Step 2.7 - Get age groups
departure <- merge(departure, age_group, by = "age", all = TRUE)
#!!!Note: Age group is only missing if age is missing.

#Step 2.8 - Process duration of stay, a lot of cleaning in required to do at data entry level
#Need to research how to use nested if statements. Use merge for now.
durStay_Away <- read_excel("data/durStay_Group.xlsx")
colnames(durStay_Away)[colnames(durStay_Away) == "durStayCalc"] <- "daysAway"
colnames(durStay_Away)[colnames(durStay_Away) == "stayAwayGroup"] <- "daysAwayGroup"
departure <- merge(departure, durStay_Away, by = "daysAway", all = TRUE) #merge files
departure$daysAwayGroup[is.na(departure$daysAwayGroup)] <- "Missing"
#departure <- departure[!is.na(departure$flightship), ] #drop empty rows
#!!!Note: There are cases where dates are entered as days away. ASO to note that this variable requires
# the number of days away. ASO to calculate this number if a date is entered in the forms.

departure$resident <- ifelse(departure$resident==1,"Resident","Visitor")
departure$sex <- ifelse(departure$sex==1,"Male","Female")
departure$transport <- ifelse(departure$transport==1,"Air","Sea")
purpTravel <- read_excel("data/purpVisit.xlsx")
colnames(purpTravel)[colnames(purpTravel) == "purpVisit"] <- "purpTravel"
colnames(purpTravel)[colnames(purpTravel) == "purpVisitDesc"] <- "purpTravelDesc"
departure <- merge(departure, purpTravel, by = "purpTravel", all = TRUE) #merge files

#Step 2.9 - Write departure to db
departure <- departure[!is.na(departure$flightship), ]
departure$N <- 1
dbWriteTable(mydb, "departure", departure, overwrite = TRUE)

#dbDisconnect(mydb)