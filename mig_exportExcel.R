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

mydb <- dbConnect(RSQLite::SQLite(), "data/migration.db")
arr <- dbGetQuery(mydb, "SELECT * FROM arrivals")
write.xlsx(arr,"C:/Users/lpele/OneDrive/Documents/CSD/03 SOCIAL/MIGRATION/2024/Q1/R_procData/Arrivals.xlsx", asTable = FALSE, overwrite = TRUE)
dep <- dbGetQuery(mydb, "SELECT * FROM departure")
write.xlsx(dep,"C:/Users/lpele/OneDrive/Documents/CSD/03 SOCIAL/MIGRATION/2024/Q1/R_procData/Departures.xlsx", asTable = FALSE, overwrite = TRUE)