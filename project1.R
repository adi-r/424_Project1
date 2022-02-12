# Directory
setwd('C:\\Users\\aranga22\\Downloads\\Academics\\Sem 2\\424 Visual Data\\Projects\\424_Project1')
getwd()

# Libraries
library(lubridate)
library(dplyr)
library(ggplot2)
# Read Table
cta_station <- read.table(file = 'cta_station_entries.tsv', sep = '\t', header = TRUE, quote="\"")

# Basic stats
str(cta_station)
summary(cta_station)
dim(cta_station)
head(cta_station)

# Data Pre-Processing
# date fix
cta_station$date <- as.Date(cta_station$date, '%m/%d/%Y')
str(cta_station)
# week_day, m, d, Y extraction
cta_station$year <- as.numeric(format(cta_station$date, format='%Y'))
cta_station$month <- as.numeric(format(cta_station$date, format='%m'))
cta_station$day <- as.numeric(format(cta_station$date, format='%d'))
cta_station$week_day <- wday(cta_station$date, label=TRUE)

# Add Month name
name_of_month <- function(df){
  df$month_name[df$month==1] <- 'Jan'
  df$month_name[df$month==2] <- 'Feb'
  df$month_name[df$month==3] <- 'Mar'
  df$month_name[df$month==4] <- 'Apr'
  df$month_name[df$month==5] <- 'May'
  df$month_name[df$month==6] <- 'Jun'
  df$month_name[df$month==7] <- 'Jul'
  df$month_name[df$month==8] <- 'Aug'
  df$month_name[df$month==9] <- 'Sep'
  df$month_name[df$month==10] <- 'Oct'
  df$month_name[df$month==11] <- 'Nov'
  df$month_name[df$month==12] <- 'Dec'
  
  return(df)
  
}
cta_station <- name_of_month(cta_station)

# Extract Relevant Station Data
# UIC
uic_station <- subset(cta_station, stationname == 'UIC-Halsted')
glimpse(uic_station)

ok <- complete.cases(uic_station)
dim(uic_station[ok, ]) == dim(uic_station)

# O'Hare Airport
ohare_station <- subset(cta_station, stationname == "O'Hare Airport")
glimpse(ohare_station)

ok <- complete.cases(ohare_station)
dim(ohare_station[ok, ]) == dim(ohare_station)

# Racine
racine_station <- subset(cta_station, stationname == "Racine")
glimpse(racine_station)

ok <- complete.cases(racine_station)
dim(racine_station[ok, ]) == dim(racine_station)

# Merge Station data
station_data <- do.call("rbind", list(uic_station, ohare_station, racine_station))
names(station_data)[1] <- 'station id'
str(station_data)

# Write data to .tsv file
write.table(station_data, file='station_data.tsv', quote=FALSE, sep='\t', col.names = NA)

# Load and verify written data
trial <- read.table(file="station_data.tsv", sep="\t", header=TRUE, quote="\"", )
trial <- trial[, -1]
str(trial)

################################################################################
# no_of_chunks <- 10
# f <- ceiling(1:nrow(station_data) / nrow(station_data) * 10)
# res <- split(station_data, f)
# map2(res, paste0("chunk_", names(res), ".tsv"), write.csv)
################################################################################

ggplot(data = uic_station[uic_station$year==2021,], aes(x = date, y = rides)) +
  geom_bar(stat = "identity", aes(fill = rides), fill = "red" )+
  labs(x = "Date", y ="Rides", title = "Station Rides Across the Years")
trial
new_data <- trial[trial$year == 2021,]
new_data <- new_data[c("date", "rides")]
new_data
