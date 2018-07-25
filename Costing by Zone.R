library(tidyverse)
library(pryr)
library("readxl")

#Choose the GTFS to unzip
zip <- file.choose()
#Create the directory to unzip it and set it as working directory
outDir <- substring(zip, 1, nchar(zip)-4)
setwd(outDir)
#Unzip it and put it in the directory
unzip(zip, exdir = outDir)

#Reading the files and creating the dataframes
trips <- read_csv("trips.txt")
#stop_times <- read.csv("stop_times.txt", colClasses=c(arrival_time ="character", departure_time = 'character'))
#read_csv(examplecsv, col_types = cols(b = col_character()))
stop_times <- read_csv("stop_times.txt", col_types= cols(arrival_time = col_character(), departure_time = col_character()))
stops <- read_csv("stops.txt")
shapes <- read_csv("shapes.txt")
routes <- read_csv("routes.txt")
regions <- read_excel("/Users/santiagotoso/Google Drive/Master/Curso R/GTFS/Stops_and_stations_20180701.xlsx")

#I'll only keep the columns that are interesting to me in the regions file
regions_short <- regions %>% 
  select(TariffZoneKey,
         TariffZoneDisplayName,
         TariffZoneNumber, 
         MunicipalityDisplayName,
         MunicipalityCode, 
         StopPointId, 
         StopPointDisplayName, 
         StopPointNumber,
         StopPointName, 
         StopPointTypeCode, 
         IsCurrent) %>% 
  filter(IsCurrent == 1) %>% 
  filter(StopPointTypeCode == 'BUSSTOP') %>% 
  select(-StopPointTypeCode, - IsCurrent)

#Join everything with the stop_times dataframe
stop_times <- stop_times %>% 
  left_join(trips) %>%
  left_join(stops) %>% 
  mutate(StopPointNumber = as.numeric(stop_id)) %>% 
  left_join(regions_short) %>% 
  select(-block_id) 

summary(stop_times)

# Export the existing stops in stop_times that where not found in
# the regions document
export <- stop_times %>% 
  filter(is.na(TariffZoneKey) == TRUE) %>% 
  select(stop_id) %>% 
  unique()

write_csv(export, "/Users/santiagotoso/Downloads/Movia_Stops_not_found")

#See when we change trip and tarif zone
trips_ids <- stop_times %>%
  select(trip_id, TariffZoneNumber)

summary(trips_ids)

trips_ids <- trips_ids %>% 
  #trip_id from the previous stop
  mutate(prev_trip = ifelse(is.na(lag(trip_id)), 
                            trips_ids[nrow(trips_ids), 1], 
                            lag(trip_id))) %>% 
  #trip_id from the next stop
  mutate(next_trip = ifelse(is.na(lead(trip_id)), 
                            trips_ids[1, 1], 
                            lead(trip_id))) %>% 
  #TariffZoneNumber of the previous stop
  mutate(prev_zone = ifelse(is.na(lag(TariffZoneNumber)),
                            TariffZoneNumber,
                            lag(TariffZoneNumber)) ) %>% 
  #TariffZoneNumber of the next stop
  mutate(next_zone = ifelse(is.na(lead(TariffZoneNumber)),
                            TariffZoneNumber,
                            lead(TariffZoneNumber)) )

summary(trips_ids)

#Store the important stop_time in a new data frame
breaks <- stop_times[(
  trips_ids$trip_id != trips_ids$prev_trip | 
    trips_ids$trip_id != trips_ids$next_trip |
    trips_ids$TariffZoneNumber != trips_ids$prev_zone |
    trips_ids$TariffZoneNumber != trips_ids$next_zone 
), ]

#Check for NA values
summary(breaks)

#Looks like we have more NAs in some field than other, let's see if every NA 
#in TariffZoneNumber is a NA in trip_id
breaks_na <- breaks %>%
  select(trip_id, stop_id, TariffZoneNumber) %>% 
  filter(is.na(TariffZoneNumber) == 'TRUE')

summary(breaks_na)

#it is. Lets filter those numbers out
breaks <- breaks %>% 
  filter(is.na(TariffZoneNumber) != 'TRUE')

summary(breaks)

#calculate running time for each
hours <- as.integer(substr(breaks$arrival_time, 1, 2))
minutes <- as.integer(substr(breaks$arrival_time, 4, 5))/60

breaks <- breaks %>% 
  mutate(hours_minutes = hours + minutes) %>% 
  mutate(travel_hours = ifelse(trip_id == lead(trip_id), (lead(hours_minutes) - hours_minutes), 0)) %>% 
  select(-timepoint, - direction_id, -hours_minutes)

breaks2 <- breaks[, c('stop_id', 'shape_id', 'stop_lat', 'stop_lon')]

breaks2[, 'uniquevalues'] <- paste(breaks2$stop_id, breaks2$shape_id, sep = '-' )
length(unique(breaks2$uniquevalues))

#Rescue the last value
breaks[nrow(breaks), 'travel_hours'] <- 0


#Summarise the information by trip and route for each region
by_trip_region <- breaks %>%
  group_by(trip_id, TariffZoneDisplayName, service_id) %>% 
  summarise(travel_hours = sum(travel_hours)) %>%
  left_join(trips) %>% 
  select(trip_id,TariffZoneDisplayName,travel_hours,route_id, service_id) %>% 
  left_join(routes) %>% 
  select(route_long_name,trip_id,TariffZoneDisplayName,travel_hours,service_id,-route_id)

#export2 <- by_trip_region %>% 
 # filter(route_long_name == '10')

#write_csv(export2, "/Users/santiagotoso/Downloads/check")

#export3 <- stop_times %>% 
 # filter(route_id=='5205fc6') %>% 
#  select(trip_id,stop_id,stop_sequence, arrival_time, TariffZoneDisplayName, service_id)

# write_csv(export3, "/Users/santiagotoso/Downloads/route10")

#head(by_trip_region)
#head(breaks[,c(1,3,4,15)], 8)

by_route_region <- by_trip_region %>%
  group_by(route_long_name, TariffZoneDisplayName) %>% 
  summarise(travel_hours = sum(travel_hours)) 

#RIGHT NOW IT'S GOING TO THE ORIGIN REGION BUT THIS COULD BE EASILY CHANGED
#IF I CHANGE THE FUNCTION LEAD FOR LAG WHEN I CALCULATE THE TRAVEL_TIMES
#I'M NOT CONSIDERING KM HERE, ONLY TIMES