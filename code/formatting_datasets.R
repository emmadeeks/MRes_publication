rm(list=ls()) #Clear global environment 
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/Publication/data/shape_files/")

library(raster) #Require these packages 
library(sf)     #Require 
library(viridis)
library('units')
library('rgdal')

require(ggmap)
require(rgdal)
require(sf)
library(data.table)
library(geosphere)
library(tidyverse)
library(lubridate)
library(plyr)
library(ggplot2)

##### Reading in shape files 
chagos_v6 <- read_sf(dsn = ".", layer = "Chagos_v6") #read in the shapefiles


tracts <- readOGR(dsn = ".", layer = "ChagosEEZ") %>%
  spTransform("+proj=longlat +ellps=WGS84")
Chagos_try <- fortify(tracts)

chagos_v6 <- readOGR(dsn = ".", layer = "Chagos_v6") %>%
  spTransform("+proj=longlat +ellps=WGS84")
Chagos_island <- fortify(chagos_v6)

# Reading in raw data to process into comparable datasets 

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/Publication/data") #go to the data directory 

################# acoustic and BPV 1 raw data from drop box 
acoustic <- read.table("/Users/emmadeeks/Dropbox/Overlap_data/Chagos_ALL_acoustic_2019.txt", header = TRUE, sep = ",", dec = ".") #read in the data 
BPV <- read.csv("/Users/emmadeeks/Dropbox/Overlap_data/BPV Pacific Marlin 2013-2016.csv", header = TRUE) # read in the data

##### Round BPV data by the hour 
BPV$date <- dmy_hms(BPV$Date)
BPV$Date <- round_date(BPV$date, unit = "hour")

# rename some columns and just make dataframe with dares and latitude 
toadd = as.data.frame(matrix(nrow = 1, ncol = 3))
col <- c("Date", "lon", "lat")
colnames(toadd) <- col




############################# calculating mid long and lat for pacific marlin BPV vessel 

# For loop for interpolating longitudes and latitudes 

for (i in 1:nrow(BPV)){
  row1 <- BPV[i,]
  row2 <- BPV[i+1,]
  midpoint <- midPoint(c(row1$Longitude, row1$Latitude), c(row2$Longitude, row2$Latitude))
  date <- row1$Date + 1*60*60
  whole <- cbind(as.character(date), midpoint)
  colnames(whole) <- col
  toadd <- rbind(toadd, whole)
  
}

toadd <- toadd[-1,]

# Combinding the actual and interpolated longitudes and latitudes 
cols_BPV <- c("Date","Longitude", "Latitude")
new_BPV <- data.table(BPV$Date, BPV$Longitude, BPV$Latitude)
toadd_table <- data.table(toadd)
colnames(new_BPV) = cols_BPV
colnames(toadd_table) = cols_BPV

toadd_table$Date <- as.POSIXct(toadd_table$Date, format="%Y-%m-%d %H:%M:%S")


#This is now a datatable with the mid points for the data frame 
BPV_moretimes <- rbindlist(list(new_BPV,toadd_table), use.names=TRUE)

#Second BPV dataset which is every 5 minutes 

BPV2 <- read.csv("/Users/emmadeeks/Dropbox/Overlap_data/BPV_Position_Data_Combined_2017_2019.csv", header = TRUE, stringsAsFactors = T) # read in the data

cols_BPV <- c("Date","Longitude", "Latitude")
BPV2$newDate <- paste(BPV2$Date, BPV2$Time)
BPV2$Date <- paste(BPV2$Date, BPV2$Time)
new_BPV_2 <- data.table(BPV2$Date, BPV2$DEC_LON, BPV2$DEC_LAT)
colnames(new_BPV_2) = cols_BPV

new_BPV_2$Date <- dmy_hms(new_BPV_2$Date)
new_BPV_2$Date <- round_date(new_BPV_2$Date, unit = "hour")
new_uniq <- new_BPV_2[!duplicated(new_BPV_2[,1]),] # every hour 


formatted_BPV2 <- rbind(BPV_moretimes, new_uniq)

################## This is the one being used for the analysis as it excludes diego garcia 
formatted_BPV2$Longitude <- as.numeric(levels(formatted_BPV2$Longitude))[formatted_BPV2$Longitude]
formatted_BPV2$Latitude <- as.numeric(levels(formatted_BPV2$Latitude))[formatted_BPV2$Latitude]

DG <- formatted_BPV2%>% filter(between(Latitude, -7.6, -7.0))
within_dg <- DG%>% filter(between(Longitude, 72.3, 72.5))

formatted_BPV2_nodg <- formatted_BPV2[!(formatted_BPV2$Date %in% within_dg$Date),]

write.csv(formatted_BPV2_nodg, "BPV_hour_NO_dg_sept.csv")
write.csv(formatted_BPV2, "BPV_hour_INCLUDE_dg_sept.csv")



############# reading the acoustic data in and rounding up 

acoustic$detect_date <- as.POSIXct(acoustic$detect_date, format="%Y-%m-%d %H:%M:%S")
acoustic$new_date <- round_date(acoustic$detect_date, unit = "hour")


cols <- c("Date","Longitude", "Latitude", "Code", "station")
new_acoustic <- data.table(acoustic$new_date, acoustic$receiver_lon, acoustic$receiver_lat, acoustic$code, acoustic$station)
colnames(new_acoustic) = cols #assigning column names 




############################# MAKING ACOUSTIC DATA JUST ELASMIOBRANCHS #####################

acoustic <- new_acoustic
#acoustic <- read.csv("acoustic_formatted_times.csv")
tags_to_delete <- c(54879, 1344, 1334, 1338, 1790024, 1339, 1330, 28648, 28651, 54960, 27612, 27587, 54963, 14920, 2386)


for (i in 1:length(tags_to_delete)){
  i = tags_to_delete[i]
  acoustic <- acoustic[!grepl(i, acoustic$Code), ]
}


write.csv(acoustic, "acoustic_no_elas_Sept.csv")


############### SORTING OUT THE SATELLITE TAG DATASET 


GPS_all <- read.csv("satellite_tags_sept_20.csv", colClasses=c("numeric", "character", "numeric", "numeric"))
GPS_all$station <- "GPS"

cols <- c("Code", "Date", "Latitude", "Longitude", "station")
colnames(GPS_all) <- cols 

GPS_all <- data.frame(GPS_all, stringsAsFactors=FALSE)

GPS_all$Date <- dmy_hm(GPS_all$Date)
GPS_all$Date <- round_date(GPS_all$Date, unit = "hour")

############ rearranging as got the pngitudes the wrong way round 
GPS <- cbind(as.character(GPS_all$Date), GPS_all$Longitude, GPS_all$Latitude, GPS_all$Code, GPS_all$station)
GPS <- data.frame(GPS, stringsAsFactors=FALSE)
acoustic <- data.frame(acoustic, stringsAsFactors=FALSE)
cols <- c("Date","Longitude", "Latitude", "Code", "station")
colnames(GPS) <- cols 

GPS$Date <- as.POSIXct(GPS$Date, format="%Y-%m-%d %H:%M:%S")


write.csv(GPS, "formatted_satellite_Sept.csv")





################## creating a dataset of acoustic and BPV 
BPV <- formatted_BPV2_nodg
m1 <- merge(acoustic, BPV, by = "Date")

cols_10 <- c("Date", "Longitude_ac", "Latitude_ac","Code","station", "Longitude_BPV", "Latitude_BPV")

colnames(m1) = cols_10

okay <- data.table(m1)
okay$Longitude_ac <- as.numeric(okay$Longitude_ac)
okay$Latitude_ac <- as.numeric(okay$Latitude_ac)


okay$NewDate <- substr(okay$Date, 0, 10)

write.csv(okay, "acoustic_BPV_dataset.csv")

################## creating a dataset of satellite and BPV 
BPV <- formatted_BPV2_nodg
m1 <- merge(GPS, BPV, by = "Date")

cols_10 <- c("Date", "Longitude_sat", "Latitude_sat","Code","station", "Longitude_BPV", "Latitude_BPV")

colnames(m1) = cols_10

okay <- data.table(m1)
okay$Longitude_sat <- as.numeric(okay$Longitude_sat)
okay$Latitude_sat <- as.numeric(okay$Latitude_sat)


okay$NewDate <- substr(okay$Date, 0, 10)

write.csv(okay, "satellite_BPV_dataset.csv")

