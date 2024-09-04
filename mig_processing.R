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
arrivals$yearMonth <- paste0(arrivals$year,"-",arrivals$month)

#Step 1.2 - Get country description, region code, and region description
country_region <- read_excel("data/country_region.xlsx") #load country and region file
arrivals <- merge(arrivals, country_region, by = "countryCode", all = TRUE) #merge files
arrivals <- arrivals[!is.na(arrivals$flightship), ] #drop empty rows
arrivals$countryCode[is.na(arrivals$countryCode)] <- "Missing"
arrivals$countryName[is.na(arrivals$countryName)] <- "Missing"
arrivals$regionCode[is.na(arrivals$regionCode)] <- "Missing"
arrivals$regionName[is.na(arrivals$regionName)] <- "Missing"

#Step 1.3 - Generate resident status
arrivals$resident <- ifelse(arrivals$purpVisit==1,1,2)
arrivals$resident <- ifelse(arrivals$countryCode==3609,1,2)
#!!!Note: ASO to check residency. Tuvalu nationals should be residents. There are cases when this is not true.
# Classifying such cases as non-resident should be carefully considered.

#Step 1.4 - Calculate age
arrivals$dobYear <- year(arrivals$dob)
arrivals$age <- arrivals$year - arrivals$dobYear

#imputing missing age
#Mean age by year and month
arrivalsMeanAgeYM <- arrivals
arrivalsMeanAgeYM <- arrivalsMeanAgeYM[!is.na(arrivalsMeanAgeYM$age), ]
arrivalsMeanAgeYM <- arrivalsMeanAgeYM%>%
  group_by(yearMonth)%>%
  summarise(meanAgeYM = round(mean(age),0))
arrivals <- merge(arrivals, arrivalsMeanAgeYM, by = "yearMonth", All = TRUE)

#Mean age by date
arrivalsMeanAgeDate <- arrivals
arrivalsMeanAgeDate <- arrivalsMeanAgeDate[!is.na(arrivalsMeanAgeDate$age), ]
arrivalsMeanAgeDate <- arrivalsMeanAgeDate%>%
  group_by(dateArrival)%>%
  summarise(meanAgeDate = round(mean(age),0))
arrivals <- merge(arrivals, arrivalsMeanAgeDate, by = "dateArrival", All = TRUE)

#Mean age by year
arrivalsMeanAgeY <- arrivals
arrivalsMeanAgeY <- arrivalsMeanAgeY[!is.na(arrivalsMeanAgeY$age), ]
arrivalsMeanAgeY <- arrivalsMeanAgeY%>%
  group_by(year)%>%
  summarise(meanAgeY = round(mean(age),0))
arrivals <- merge(arrivals, arrivalsMeanAgeY, by = "year", All = TRUE)

arrivals$corrAge <- ifelse(is.na(arrivals$age), arrivals$meanAgeDate, arrivals$age)
arrivals$corrAge <- ifelse(is.na(arrivals$corrAge), arrivals$meanAgeYM, arrivals$corrAge)
arrivals$corrAge <- ifelse(is.na(arrivals$corrAge), arrivals$meanAgeY, arrivals$corrAge)
arrivals$corrAge <- ifelse(arrivals$corrAge <0 | arrivals$corrAge>99, arrivals$meanAgeDate, arrivals$corrAge)
arrivals$corrAge <- ifelse(arrivals$corrAge <0 | arrivals$corrAge>99, arrivals$meanAgeYM, arrivals$corrAge)
arrivals$corrAge <- ifelse(arrivals$corrAge <0 | arrivals$corrAge>99, arrivals$meanAgeY, arrivals$corrAge)

#Do final check to ensure there are no missing or erroneous corrected ages
arrivals$corrAgeChk <- ifelse(arrivals$corrAge > 99 | arrivals$corrAge < 0, "Err","Ok")
arrivals$corrAgeChk[is.na(arrivals$corrAge)] <- "Mis"

#Step 1.5 - Get age groups, sex, duration away, resident status, transport, 
arrivals$dateDep <- ymd(arrivals$durStay)
arrivals$durStayCalc <- arrivals$dateDep - arrivals$dateArrival

arrivals$purpVisit <- ifelse(arrivals$purpVisit == 10,20,arrivals$purpVisit)

arrivals <- arrivals |>
  mutate(myAgeGroup = case_when(
    corrAge <= 10 ~ "0 to 10",
    corrAge <= 20 ~ "11 to 20",
    corrAge <= 30 ~ "21 to 30",
    corrAge <= 40 ~ "31 to 40",
    corrAge <= 50 ~ "41 to 50",
    corrAge <= 60 ~ "51 to 60",
    corrAge <= 70 ~ "61 to 70",
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
arrivals$myAgeGroup <- ifelse(arrivals$corrAgeChk == "Err","Error",arrivals$myAgeGroup)
arrivals$myAgeGroup <- ifelse(arrivals$corrAgeChk == "Mis","Mis",arrivals$myAgeGroup)

#Checking duration of stay calculation and duration of stay group
arrivals$durStayCalc[is.na(arrivals$durStayCalc)] <- 8888
arrivals$durStayCalc <- ifelse(arrivals$durStayCalc <= 0,9999, arrivals$durStayCalc)
arrivals$durStayGroup <- ifelse(arrivals$durStayCalc == 8888, "Missing", arrivals$durStayGroup)
arrivals$durStayGroup <- ifelse(arrivals$durStayCalc == 9999, "Error", arrivals$durStayGroup)
arrivals$durStayCode <- ifelse(arrivals$durStayCalc == 8888| arrivals$durStayCalc == 9999,8,arrivals$durStayCode)

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
departure$yearMonth <- paste0(departure$year,"-",departure$month)
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

#imputing missing and erroneous ages
#Mean age by year month
departureMeanAgeYM <- departure
departureMeanAgeYM <- departureMeanAgeYM[!is.na(departureMeanAgeYM$age), ]
departureMeanAgeYM <- departureMeanAgeYM%>%
  group_by(yearMonth)%>%
  summarise(meanAgeYM = round(mean(age),0))
departure <- merge(departure, departureMeanAgeYM, by = "yearMonth", All = TRUE)

#Mean age by date
departureMeanAgeDate <- departure
departureMeanAgeDate <- departureMeanAgeDate[!is.na(departureMeanAgeDate$age), ]
departureMeanAgeDate <- departureMeanAgeDate%>%
  group_by(dateDeparture)%>%
  summarise(meanAgeDate = round(mean(age),0))
departure <- merge(departure, departureMeanAgeDate, by = "dateDeparture", All = TRUE)

#Mean age by year
departureMeanAgeY <- departure
departureMeanAgeY <- departureMeanAgeY[!is.na(departureMeanAgeY$age), ]
departureMeanAgeY <- departureMeanAgeY%>%
  group_by(year)%>%
  summarise(meanAgeY = round(mean(age),0))
departure <- merge(departure, departureMeanAgeY, by = "year", All = TRUE)

departure$corrAge <- ifelse(is.na(departure$age), departure$meanAgeDate, departure$age)
departure$corrAge <- ifelse(is.na(departure$corrAge), departure$meanAgeYM, departure$corrAge)
departure$corrAge <- ifelse(is.na(departure$corrAge), departure$meanAgeY, departure$corrAge)
departure$corrAge <- ifelse(departure$corrAge <0 | departure$corrAge>99, departure$meanAgeDate, departure$corrAge)
departure$corrAge <- ifelse(departure$corrAge <0 | departure$corrAge>99, departure$meanAgeYM, departure$corrAge)
departure$corrAge <- ifelse(departure$corrAge <0 | departure$corrAge>99, departure$meanAgeY, departure$corrAge)

#Do final check to ensure there no missing or erroneous ages
departure$corrAgeChk[is.na(departure$corrAge)] <- "Mis"
departure$corrAgeChk <- ifelse(departure$corrAge <0 | departure$corrAge>99, "Err", "Ok")

#Step 1.5 - Get age groups, sex, duration away, resident status, transport, 
departure <- departure |>
  mutate(myAgeGroup = case_when(
    corrAge <= 10 ~ "0 to 10",
    corrAge <= 20 ~ "11 to 20",
    corrAge <= 30 ~ "21 to 30",
    corrAge <= 40 ~ "31 to 40",
    corrAge <= 50 ~ "41 to 50",
    corrAge <= 60 ~ "51 to 60",
    corrAge <= 70 ~ "61 to 70",
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

departure$myAgeGroup <- ifelse(departure$corrAgeChk == "Err", "Err",departure$myAgeGroup)
departure$myAgeGroup <- ifelse(departure$corrAgeChk == "Mis", "Mis",departure$myAgeGroup)

#Checking days away calculations and days away group assignment
departure$daysAway[is.na(departure$daysAway)] <- 8888
departure$daysAway<- ifelse(departure$daysAway <= 0,9999, departure$daysAway)
departure$stayAwayGroup <- ifelse(departure$daysAway == 8888, "Missing", departure$stayAwayGroup)
departure$stayAwayGroup <- ifelse(departure$daysAway == 9999, "Error", departure$stayAwayGroup)

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
match$chk <- ifelse(is.na(match$dateArrival) & !is.na(match$dateDeparture), "Check Arrival", "Ok")
match$chk <- ifelse(!is.na(match$dateArrival) & is.na(match$dateDeparture), "Check Departure",match$chk)
dbWriteTable(mydb, "match", match, overwrite = TRUE)

dbDisconnect(mydb)

#source("mig_tabulation.R")