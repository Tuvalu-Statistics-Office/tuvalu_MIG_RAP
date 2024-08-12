#Lae Peleti
#Senior Statistician
#Central Statistics Division
#Ministry of Finance and Economic Development
#Government of Tuvalu

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Load setup file
source("R/function/setup.R")

#Connect to db
mydb <- dbConnect(RSQLite::SQLite(), "data/migration.db")

#-------------------------------------------------------------------------------------------------------
#Figure 1 - Number of visitors by nationality 2024 (Pie Chart)
#-------------------------------------------------------------------------------------------------------
arrCountry <- dbGetQuery(mydb, "SELECT selectedCountry AS Country, 
                                        sum(N) AS Count 
                                 FROM arrivals 
                                 WHERE year = 2024 and resident = 2
                                 GROUP BY Country")
totCountry <- dbGetQuery(mydb, "SELECT year, 
                                      sum(N) AS Value 
                               FROM arrivals 
                               WHERE Year = 2024 AND resident = 2
                               GROUP BY Year")

arrCountry$Total <- totCountry$Value
arrCountry$Percent <- round((arrCountry$Count / arrCountry$Total) * 100, 2)
countryTB <<- arrCountry %>%
  select(Country, Count, Percent) %>%
  arrange(desc(Percent))
Country_pie <- ggplot(countryTB, aes(x = "", y = Percent, fill = Country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Visitors by Country")
ggsave("image/country_pie.png", plot = Country_pie, width = 6, height = 6, units = "in")

#-------------------------------------------------------------------------------------------------------
#Figure 2 - Number of visitors by nationality by month (Column graph)
#-------------------------------------------------------------------------------------------------------
arrCountry <- dbGetQuery(mydb, "SELECT month AS Month,
                                        sum(N) AS Count 
                                 FROM arrivals 
                                 WHERE year = 2024 and resident = 2
                                 GROUP BY Month")
countryTB <<- arrCountry %>%
  select(Month, Count)
countryGraph <- ggplot(countryTB, aes(x = Month, y = Count)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(title = "Visitors by Nationality by Month- 2024", x = "Month", y = "Number of visitors") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("image/visitorsByCountryByMonth.png", plot = countryGraph, width = 7, height = 8, units = "in")

#-------------------------------------------------------------------------------------------------------
#Figure 3 - Number of visitors by month (Line graph)
#-------------------------------------------------------------------------------------------------------
arrCountry <- dbGetQuery(mydb, "SELECT year AS Year,
                                        sum(N) AS Count 
                                 FROM arrivals 
                                 WHERE resident = 2
                                 GROUP BY Year")
countryTB <<- arrCountry %>%
  select(Year, Count)
countryTB$grp <- "Year"
visitorsYear <- ggplot(countryTB, aes(x = Year, y = Count, group = grp)) +
  geom_point(colour = "red", size = 5) +
  geom_line(colour = "red", linewidth = 2) +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Visitors by years", x = "Year", y = "Number of Visitors")
ggsave("image/visitorsYear.png", plot = visitorsYear, width = 6, height = 5, units = "in")
#-------------------------------------------------------------------------------------------------------
#Figure 4 - Number of arrivals by residence by year (Column graph) !!Need more work
#-------------------------------------------------------------------------------------------------------
arrCountry <- dbGetQuery(mydb, "SELECT year AS Year, resident AS Residence, N AS Count
                                FROM arrivals")
countryTB <<- arrCountry %>%
  select(Year, Residence, Count)
countryTB$grp <- countryTB$Residence
countryGraph <- ggplot(countryTB, aes(x = Year, y = Count, group=grp)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(title = "Arrivals by Residence by Year", x = "Year", y = "Number of arrivals") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("image/arrivalsYearResidence.png", plot = countryGraph, width = 7, height = 8, units = "in")