#Lae Peleti
#Senior Statistician
#Central Statistics Division
#Ministry of Finance and Economic Development
#Government of Tuvalu

#Load required libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(RSQLite)
library(lubridate) #Date conversions and manipulations
library(officer)
library(tidyverse)
library(ggplot2)
library(pivottabler)

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Connect to db
mydb <- dbConnect(RSQLite::SQLite(), "data/migration.db")
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
#--------------------------------------------------------------------------------------------------------------
#Generating arrival tables
#--------------------------------------------------------------------------------------------------------------
#Table 1: Visitors arrivals by nationality, month, and sex
#--------------------------------------------------------------------------------------------------------------
#Step 1 - Get countries with frequency >= 50
arrCountry <- dbGetQuery(mydb, "SELECT countryName, 
                                        sum(N) AS Count 
                                 FROM arrivals 
                                 WHERE year > 2000
                                 GROUP BY countryName")
arrCountry$Selected <- ifelse(arrCountry$Count>=50,1,2)
arrivals <- merge(arrivals, arrCountry, by = "countryName", all = TRUE)
arrivals$selectedCountry <- ifelse(arrivals$Selected==1,arrivals$countryName,"Other")
dbWriteTable(mydb, "arrivals", arrivals, overwrite = TRUE)

arrCountry <- dbGetQuery(mydb, "SELECT * FROM arrivals WHERE year = 2024 AND resident = 'Visitor'")

pt <- PivotTable$new()
pt$addData(arrCountry)
pt$addColumnDataGroups("month")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("selectedCountry")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableA1")
pt$writeToExcelWorksheet(wb=wb, wsName="tableA1", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 2 - Visitors arrivals by purpose of visit, month, and sex
#--------------------------------------------------------------------------------------------------------------
pt <- PivotTable$new()
pt$addData(arrCountry)
pt$addColumnDataGroups("month")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("purpVisitDesc")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableA2")
pt$writeToExcelWorksheet(wb=wb, wsName="tableA2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 3 - Visitors arrivals by nationality, purpose of visit, and sex
#--------------------------------------------------------------------------------------------------------------
pt <- PivotTable$new()
pt$addData(arrCountry)
pt$addColumnDataGroups("purpVisitDesc")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("selectedCountry")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableA3")
pt$writeToExcelWorksheet(wb=wb, wsName="tableA3", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 4 - Resident arrivals by nationality, month, and sex
#--------------------------------------------------------------------------------------------------------------
tableA1 <- dbGetQuery(mydb, "SELECT * FROM arrivals WHERE resident = 'Resident' AND year = 2024")
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("month")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("countryName")
pt$defineCalculation(calculationName="TotalResidents", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableA4")
pt$writeToExcelWorksheet(wb=wb, wsName="tableA4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 5 - Total arrivals by resident status, month, transport mode, and sex
#--------------------------------------------------------------------------------------------------------------
tableA1 <- dbGetQuery(mydb, "SELECT * FROM arrivals WHERE year = 2024")
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("transport")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("resident")
pt$addRowDataGroups("month")
pt$defineCalculation(calculationName="TotalArrivals", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableA5")
pt$writeToExcelWorksheet(wb=wb, wsName="tableA5", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 6 - Visitors arrivals by duration of stay and months
#--------------------------------------------------------------------------------------------------------------
tableA1 <- dbGetQuery(mydb, "SELECT * FROM arrivals WHERE resident = 'Visitor' AND year = 2024")
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("month")
pt$addRowDataGroups("stayAwayGroup")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableA6")
pt$writeToExcelWorksheet(wb=wb, wsName="tableA6", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 7 - Total arrivals by resident status, age group, and month by sex
#--------------------------------------------------------------------------------------------------------------
tableA1 <- dbGetQuery(mydb, "SELECT * FROM arrivals WHERE year = 2024")
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("month")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("resident")
pt$addRowDataGroups("ageGroup")
pt$defineCalculation(calculationName="TotalArrivals", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableA7")
pt$writeToExcelWorksheet(wb=wb, wsName="tableA7", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 8 - Total arrivals by months by resident status and sex
#--------------------------------------------------------------------------------------------------------------
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("resident")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("month")
pt$defineCalculation(calculationName="TotalArrivals", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableA8")
pt$writeToExcelWorksheet(wb=wb, wsName="tableA8", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

saveWorkbook(wb, file="output/Arrival Tables.xlsx", overwrite = TRUE)

#--------------------------------------------------------------------------------------------------------------
#Generating departure tables
#--------------------------------------------------------------------------------------------------------------
#Table 1: Total departure by month and resident status by sex
#--------------------------------------------------------------------------------------------------------------
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))

tableA1 <- dbGetQuery(mydb, "SELECT * FROM departure WHERE year = 2024")
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("resident")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("month")
pt$defineCalculation(calculationName="TotalDepartures", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD1")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD1", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 2: Total departure by resident status by month and transport mode by sex
#--------------------------------------------------------------------------------------------------------------
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("transport")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("resident")
pt$addRowDataGroups("month")
pt$defineCalculation(calculationName="TotalDepartures", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD2")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 3: Total departure by resident status by month and age group mode by sex
#--------------------------------------------------------------------------------------------------------------
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("ageGroup")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("resident")
pt$addRowDataGroups("month")
pt$defineCalculation(calculationName="TotalDepartures", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD3")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD3", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 4: Total departure by months and purpose of travel by sex
#--------------------------------------------------------------------------------------------------------------
pt <- PivotTable$new()
pt$addData(tableA1)
pt$addColumnDataGroups("purpTravelDesc")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("month")
pt$defineCalculation(calculationName="TotalDepartures", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD4")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 5: Resident departure by months and purpose of travel by sex
#--------------------------------------------------------------------------------------------------------------
tableA2 <- dbGetQuery(mydb, "SELECT * FROM departure WHERE year = 2024 and resident = 'Resident'")
pt <- PivotTable$new()
pt$addData(tableA2)
pt$addColumnDataGroups("purpTravelDesc")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("month")
pt$defineCalculation(calculationName="TotalResidents", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD5")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD5", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 6: Resident departure by nationality and month by sex
#--------------------------------------------------------------------------------------------------------------
pt <- PivotTable$new()
pt$addData(tableA2)
pt$addColumnDataGroups("month")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("countryName")
pt$defineCalculation(calculationName="TotalResidents", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD6")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD6", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 7: Resident departure by days away group and month by sex
#--------------------------------------------------------------------------------------------------------------
pt <- PivotTable$new()
pt$addData(tableA2)
pt$addColumnDataGroups("month")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("daysAwayGroup")
pt$defineCalculation(calculationName="TotalDepartures", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD7")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD7", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#--------------------------------------------------------------------------------------------------------------
#Table 8: Visitors departure by region and month by sex
#--------------------------------------------------------------------------------------------------------------
tableA3 <- dbGetQuery(mydb, "SELECT * FROM departure WHERE resident = 'Visitor' AND year = 2024")
pt <- PivotTable$new()
pt$addData(tableA3)
pt$addColumnDataGroups("month")
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("regionName")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD8")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD8", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
saveWorkbook(wb, file="output/Departure Tables.xlsx", overwrite = TRUE)

#--------------------------------------------------------------------------------------------------------------
#Exporting table that matches arrival and departure dates
#--------------------------------------------------------------------------------------------------------------

tab1 <- dbGetQuery(mydb,"SELECT * FROM match")
tab1$dateArrival <- convertToDateTime(tab1$dateArrival, origin = "1970-01-01")
tab1$dateDeparture <- convertToDateTime(tab1$dateDeparture, origin = "1970-01-01")
write.xlsx(tab1,"output/ArrDep_Match.xlsx", asTable = FALSE, overwrite = TRUE)

#--------------------------------------------------------------------------------------------------------------
#Exporting tables that for population estimates
#--------------------------------------------------------------------------------------------------------------
#Note: ASO to change the year and the range for month
tab2 <- dbGetQuery(mydb,"SELECT year, month, age, sex, N FROM arrivals WHERE resident = 'Resident' AND year = 2024 AND month>=1 AND month<=3")
pt <- PivotTable$new()
pt$addData(tab2)
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("age")
pt$defineCalculation(calculationName="Resident_Arrivals", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "Res_Arr")
pt$writeToExcelWorksheet(wb=wb, wsName="Res_Arr", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

tab3 <- dbGetQuery(mydb,"SELECT year, month, age, sex, N FROM departure WHERE resident = 'Resident' AND year>=2024 AND month >=1 AND month<=3")
pt <- PivotTable$new()
pt$addData(tab3)
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("age")
pt$defineCalculation(calculationName="Resident_Departures", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "Res_Dep")
pt$writeToExcelWorksheet(wb=wb, wsName="Res_Dep", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
saveWorkbook(wb, file="output/pop_est_mig.xlsx", overwrite = TRUE)
dbDisconnect(mydb)