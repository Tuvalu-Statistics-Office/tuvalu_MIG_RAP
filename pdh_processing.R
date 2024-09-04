
#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup file
source("R/function/setup.R")

#Connect to db
mydb <- dbConnect(RSQLite::SQLite(), "data/migration.db")

arrivals <- dbGetQuery(mydb, "SELECT * FROM arrivals")

arrivals <- arrivals |>
  mutate(visitor_type = case_when(
    resident == 'Resident' ~ "EXC",
    resident == 'Visitor' ~ "TOU"
  ),
  yearMonth =  paste0(year,'-',sprintf("%02d", arrivals$month))
  
  )

#### *************************************** Annual Migration processing ***************************************** ####

#Yearly visitors processing
annual_summary <- arrivals |>
  group_by(year, visitor_type) |>
  summarise(yrTotal = n())

annual_summary <- as.data.table(annual_summary)

annual_summary_cube <- cube(annual_summary, j = round(sum(yrTotal),0), by = c("year", "visitor_type"), id = FALSE)
annual_summary_cube <- annual_summary_cube[!is.na(annual_summary_cube$year)]

#Replace NA with _T
annual_summary_cube <- replace(annual_summary_cube, is.na(annual_summary_cube), "_T")

#Add new columns and rename columns
annual_summary_cube <- annual_summary_cube |>
  mutate(
    FREQ = "A",
    GEO_PICT = "TV",
    INDICATOR = "NOSVA",
    UNIT_MEASURE = "",
    UNIT_MULT = "",
    OBS_STATUS = "",
    DATA_SOURCE = "",
    OBS_COMMENTT = ""
  ) |>
  rename(
    TIME_PERIOD = year,
    OVERSEAS_VISITORS_TYPE = visitor_type,
    OBS_VALUE = V1
  )

#Monthly visitors processing
monthly_summary <- arrivals |>
  group_by(yearMonth, visitor_type) |>
  summarise(mthTotal = n())

monthly_summary <- as.data.table(monthly_summary)

monthly_summary_cube <- cube(monthly_summary, j = round(sum(mthTotal),0), by = c("yearMonth", "visitor_type"), id = FALSE)
monthly_summary_cube <- monthly_summary_cube[!is.na(monthly_summary_cube$yearMonth)]

#Replace NA with _T
monthly_summary_cube <- replace(monthly_summary_cube, is.na(monthly_summary_cube), "_T")

#Add new columns and rename columns
monthly_summary_cube <- monthly_summary_cube |>
  mutate(
    FREQ = "M",
    GEO_PICT = "TV",
    INDICATOR = "NOSVA",
    UNIT_MEASURE = "",
    UNIT_MULT = "",
    OBS_STATUS = "",
    DATA_SOURCE = "",
    OBS_COMMENTT = ""
  ) |>
  rename(
    TIME_PERIOD = yearMonth,
    OVERSEAS_VISITORS_TYPE = visitor_type,
    OBS_VALUE = V1
  )

#Combine both annual and monthly arrivals
visitors_combine <- rbind(annual_summary_cube, monthly_summary_cube)

#Rearrange columns in proper order
visitors_combine <- visitors_combine |>
  select(FREQ, TIME_PERIOD, GEO_PICT, OVERSEAS_VISITORS_TYPE, INDICATOR, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENTT)

#Write final dataframe to csv file
write.csv(visitors_combine, "output/visitors_arrival_tv.csv", row.names = FALSE)
