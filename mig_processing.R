#Testing Push and Pull
#Lae Peleti
#Senior Statistician
#Central Statistics Division
#Ministry of Finance and Economic Development
#Government of Tuvalu

##processing arrival and departure

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup file
source("R/function/setup.R")

#Import excel files for both arrivals and departures
arrivals <- read_excel("data/arrivals.xlsx")

#Connect to db
mydb <- dbConnect(RSQLite::SQLite(), "data/migration.db")

#----------------------------------------------------------------------------------------------------------------------
#1 Processing arrival data
#----------------------------------------------------------------------------------------------------------------------

#Step 1.0 - Renaming ad deleting columns and rows
arrivals <- arrivals |>
  #filter out records with no arrival dates
  filter(!is.na(`DATE OF ARRIVAL`)) |>
  #rename the columns
  rename(
    flightship = `FLIGHT/ SHIP#`,
    transport = `TRANSPORT`,
    dateArrival = `DATE OF ARRIVAL`,
    pax = `PAX#`,
    lname = `SURNAME`,
    fname = `FIRST NAME`,
    birthCountry = `COUNTRY OF BIRTH`,
    dob = `DATE OF BIRTH`,
    occupation = `OCCUPATION`,
    countryCode = `COUNTRY OF CITIZENSHIP`,
    permAddress = `PERMANENT ADDRESS`,
    sex = `SEX`,
    passport = `PASSPORT#`,
    purpVisit = `PURPOSE OF VISIT`,
    durStay = `DURATION OF STAY`,
    othPurpose = `OTHER PURPOSE (DETAILS)`
    # continue to the rest of the fields
  ) |>
  #drop the columns that are not needed
  select(-`CITIZEN COUNTRY`,
         -`REGION CODE`,
         -`REGION NAME`,
         -`RESIDENT STATUS`,
         -`AGE`,
         -`AGE GROUP`,
         -`REGION CODE`,
         -`STAY DURATION GROUP`,
         -`Month`,
         -`MONTH/YEAR`,
         -`Quarters`,
         -`ACTUAL DOB`,
         -`year`,
         -`Expected date of De-arture`
         #Continue with the rest of the fields
  ) |>
  #creating new columns using the mutate function
  mutate(
    dateArrival = ymd(dateArrival),
    month = month(dateArrival),
    year = year(dateArrival)
  )
#!!!Note: ASO should always check the formats of the dates.

#Step 1.1 - Check for years that may be incorrect. Drop record if it cannot be corrected.
curYearA <- max(arrivals$year)
arrivals$curYearA <- curYearA
arrivals <- arrivals[arrivals$year > curYearA-10, ]

#Step 1.2 - Get country description, region code, and region description
country_region <- read_excel("data/country_region.xlsx") #load country and region file
arrivals <- merge(arrivals, country_region, by = "countryCode", all = TRUE) #merge files
arrivals <- arrivals[!is.na(arrivals$flightship), ] #drop empty rows
arrivals$countryCode[is.na(arrivals$countryCode)] <- "Missing"
arrivals$countryName[is.na(arrivals$countryName)] <- "Missing"
arrivals$regionCode[is.na(arrivals$regionCode)] <- "Missing"
arrivals$regionCode[is.na(arrivals$regionCode)] <- "Missing"

#Step 1.3 - Generate resident status
arrivals$resident <- ifelse(arrivals$purpVisit==1,1,2)
arrivals$resident <- ifelse(arrivals$countryCode==3609,1,2)
#!!!Note: ASO to check residency. Tuvalu nationals should be residents. There are cases when this is not true.
# Classifying such cases as non-resident should be carefully considered.

#Step 1.4 - Calculate age
arrivals$dobYear <- year(arrivals$dob)
arrivals$age <- arrivals$year - arrivals$dobYear
arrivals$age <- ifelse(arrivals$age > 99, "ERROR",arrivals$age)
arrivals$age[is.na(arrivals$age)] <- "Missing"

#Step 1.5 - Get age groups, sex, duration away, resident status, transport, 
arrivals$dateDep <- ymd(arrivals$durStay)
arrivals$durStayCalc <- arrivals$dateDep - arrivals$dateArrival #Need to process other records
arrivals <- arrivals |>
  mutate(myAgeGroup = case_when(
    age <= 10 ~ "0 to 10",
    age <= 20 ~ "11 to 20",
    age <= 30 ~ "21 to 30",
    age <= 40 ~ "31 to 40",
    age <= 50 ~ "41 to 50",
    age <= 60 ~ "51 to 60",
    age <= 70 ~ "61 to 70",
    TRUE ~ "71+"  
  ),
  #Changing labels for sex, resident status, and transport mode
  gender = ifelse(sex ==1, "Male", "Female"),
  resid = ifelse(resident ==1, "Resident", "Visitor"),
  transport = ifelse(transport==1, "Air", "Sea"),
  #Get duration of stay group
  durStayGroup = case_when(
    durStayCalc <= 8 ~ "<8",
    durStayCalc >8 & durStayCalc <=14 ~ "9-14",
    durStayCalc >14 & durStayCalc <=30 ~ "15-30",
    durStayCalc >30 & durStayCalc <=90 ~ "31-90",
    durStayCalc >90 & durStayCalc <=180 ~ "90-180",
    durStayCalc >180 & durStayCalc <=360 ~ "181-360",
    TRUE ~ "360+"
  ),
  monthDesc = case_when(
    month == 1 ~ "Jan",
    month == 2 ~ "Feb",
    month == 3 ~ "Mar",
    month == 4 ~ "Apr",
    month == 5 ~ "May",
    month == 6 ~ "Jun",
    month == 7 ~ "Jul",
    month == 8 ~ "Aug",
    month == 9 ~ "Sep",
    month == 10 ~ "Oct",
    month == 11 ~ "Nov",
    month == 12 ~ "Dec"
  ),
  purpVisitDesc = case_when(
    purpVisit == 1 ~ "Returning resident",
    purpVisit == 2 ~ "Holiday",
    purpVisit == 3 ~ "Transit",
    purpVisit == 4 ~ "Research/ Study",
    purpVisit == 5 ~ "Business/ Commerce",
    purpVisit == 6 ~ "Government Conference",
    purpVisit == 7 ~ "Government Business",
    purpVisit == 8 ~ "Religious",
    purpVisit == 9 ~ "Other NGO",
    purpVisit == 20 ~ "Other Purpose"
    ),
  quarter = case_when(
    month >=1 & month <=3 ~ 1,
    month >=4 & month <=6 ~ 2,
    month >=7 & month <=9 ~ 3,
    month >=10 & month <=12 ~ 4
  ),
  durStayCode = case_when(
    durStayCalc <= 8 ~ 1,
    durStayCalc >8 & durStayCalc <=14 ~ 2,
    durStayCalc >14 & durStayCalc <=30 ~ 3,
    durStayCalc >30 & durStayCalc <=90 ~ 4,
    durStayCalc >90 & durStayCalc <=180 ~ 5,
    durStayCalc >180 & durStayCalc <=360 ~ 6,
    TRUE ~ 7
  )
)
#Step 1.9 - Write arrivals to db
arrivals$N <- 1
dbWriteTable(mydb, "arrivals", arrivals, overwrite = TRUE)

#----------------------------------------------------------------------------------------------------------------------
#2 Processing departure
#----------------------------------------------------------------------------------------------------------------------
departure <- read_excel("data/departures.xlsx")

#Step 2.0 - Renaming columns in departure table

departure <- departure |>
  #filter out records with no departure dates
  filter(!is.na(`DATE OF DEPARTURE`)) |>
  #rename the columns
  rename(
    flightship = `FLIGHT/ SHIP#`,
    transport = `TM`,
    destination = `DESTINATION`,
    dateDeparture = `DATE OF DEPARTURE`,
    pax = `Pax#`,
    lname = `SURNAME`,
    fname = `FIRST NAME`,
    passport = `PASSPORT#`,
    countryCode = `NATIONALITY`,
    dob = `DATE OF BIRTH`,
    sex = `SEX`,
    resident = `RESID_ ENCE`,
    purpTravel = `POT/ POV`,
    otherPurpTravel = `Details of Other Purpose`,
    daysAway = `RESIDENT (DAYS AWAY)`,
    dateArrival = `NON_RESIDENT (DATE OF ARRIVAL)`
  ) |>
  #drop the columns that are not needed
  select(
    -`COUNTRY`,
    -`Region Code`,
    -`Region Code`,
    -`Region Name`,
    -`AGE`,
    -`AGE GROUP`,
    -`Days Away Groups`,
    -`MONTH`,
    -`MONTH/ YEAR`,
    -`year`,
    -`QTR`
  )|>
  mutate(
    dateDeparture = ymd(dateDeparture),
    month = month(dateDeparture),
    year = year(dateDeparture)
  )

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

#Step 2.5 - Checking resident status, note that resident status in departure is not a dependent variable.
departure$resident <- ifelse(departure$countryCode==3609,1,2)
departure$resident[is.na(departure$resident)] <- "Missing"
#!!!Comment previous line if there are no blank residence "Missing".
#!!!Note: ASO to check residency. Tuvalu nationals should be residents. There are cases when this is not true.
# Classifying such cases as non-resident should be carefully considered.

#Step 2.6 - Calculate age
departure$dobYear <- year(departure$dob)
departure$age <- departure$year - departure$dobYear
#!!!Note: Age is only missing if either date of arrival or date of birth is missing. In all cases, these variables
# should not be missing.

#Step 1.5 - Get age groups, sex, duration away, resident status, transport, 
departure <- departure |>
  mutate(myAgeGroup = case_when(
    age <= 10 ~ "0 to 10",
    age <= 20 ~ "11 to 20",
    age <= 30 ~ "21 to 30",
    age <= 40 ~ "31 to 40",
    age <= 50 ~ "41 to 50",
    age <= 60 ~ "51 to 60",
    age <= 70 ~ "61 to 70",
    TRUE ~ "71+"   
  ),
  #Changing labels for sex, resident status, and transport mode
  gender = ifelse(sex ==1, "Male", "Female"),
  resid = ifelse(resident ==1, "Resident", "Visitor"),
  transport = ifelse(transport==1, "Air", "Sea"),
  #Get duration of stay group
  stayAwayGroup = case_when(
    daysAway <= 8 ~ "<8",
    daysAway >8 & daysAway <=14 ~ "9-14",
    daysAway >14 & daysAway <=30 ~ "15-30",
    daysAway >30 & daysAway <=90 ~ "31-90",
    daysAway >90 & daysAway <=180 ~ "90-180",
    daysAway >180 & daysAway <=360 ~ "181-360",
    TRUE ~ "360+"
  ),
  monthDesc = case_when(
    month == 1 ~ "Jan",
    month == 2 ~ "Feb",
    month == 3 ~ "Mar",
    month == 4 ~ "Apr",
    month == 5 ~ "May",
    month == 6 ~ "Jun",
    month == 7 ~ "Jul",
    month == 8 ~ "Aug",
    month == 9 ~ "Sep",
    month == 10 ~ "Oct",
    month == 11 ~ "Nov",
    month == 12 ~ "Dec"
  ),
  purpTravelDesc = case_when(
    purpTravel == 1 ~ "Tourism",
    purpTravel == 2 ~ "Family/ Social",
    purpTravel == 3 ~ "Commerce",
    purpTravel == 4 ~ "Government",
    purpTravel == 5 ~ "Education",
    purpTravel == 6 ~ "Migrate",
    purpTravel == 7 ~ "Medical",
    purpTravel == 8 ~ "Seaman",
    purpTravel == 9 ~ "Research/ Study",
    purpTravel == 10 ~ "Work Scheme",
    purpTravel == 11 ~ "Transit",
    purpTravel == 12 ~ "NGO Business",
    purpTravel == 20 ~ "Other Purpose"
  ),
  quarter = case_when(
    month >=1 & month <=3 ~ 1,
    month >=4 & month <=6 ~ 2,
    month >=7 & month <=9 ~ 3,
    month >=10 & month <=12 ~ 4
  ),
  daysAwayCode = case_when(
    daysAway <= 8 ~ 1,
    daysAway >8 & daysAway <=14 ~ 2,
    daysAway >14 & daysAway <=30 ~ 3,
    daysAway >30 & daysAway <=90 ~ 4,
    daysAway >90 & daysAway <=180 ~ 5,
    daysAway >180 & daysAway <=360 ~ 6,
    TRUE ~ 7
  )
)
departure$myAgeGroup <- ifelse(departure$age > 99, "ERROR",departure$myAgeGroup)
departure$myAgeGroup[is.na(departure$age)] <- "Missing"
#Step 2.9 - Write departure to db
departure <- departure[!is.na(departure$flightship), ]
departure$N <- 1
dbWriteTable(mydb, "departure", departure, overwrite = TRUE)

#----------------------------------------------------------------------------------------------------------------------
#3 Matching departure and arrivals
#----------------------------------------------------------------------------------------------------------------------
arrDates <- dbGetQuery(mydb, "SELECT dateArrival FROM arrivals AS date WHERE year = curYearA")
arrDates$dateA <- convertToDateTime(arrDates$date, origin = "1970-01-01")
arrDates$yearA <- year(arrDates$dateA)
arrDates$monthA <- month(arrDates$dateA)
dbWriteTable(mydb, "arrDates", arrDates, overwrite = TRUE)
depDates <- dbGetQuery(mydb, "SELECT dateDeparture FROM departure AS date WHERE year = curYearD")
depDates$dateD <- convertToDateTime(depDates$date, origin = "1970-01-01")
depDates$yearD <- year(depDates$dateD)
depDates$monthD <- month(depDates$dateD)
dbWriteTable(mydb, "depDates", depDates, overwrite = TRUE)

match <- dbGetQuery(mydb,"SELECT * FROM arrDates FULL OUTER JOIN depDates ON arrDates.dateA = depDates.dateD GROUP BY arrDates.dateArrival, depDates.dateDeparture")
dbWriteTable(mydb, "match", match, overwrite = TRUE)

dbDisconnect(mydb)

#source("mig_tabulation.R")