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


######### OVERLAP WITH ACOUSTIC INCLUDING ALL STATIONS 


ac_BPV <- read.csv("acoustic_BPV_dataset.csv")


r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
ac_BPV$distance<-distHaversine(ac_BPV[,3:4], ac_BPV[,7:8], r=r.km)
ac_overlap <- ac_BPV[ac_BPV$distance<sep.km,]

####### VERY IMPORTANT NOT TO INCLUDE REPEATING DATA

ac_overlap <- as.data.frame(ac_overlap)

####### have decided to go with this method of subsetting as it also removes multiple station detections 
ac_overlap <- ac_overlap[!duplicated(ac_overlap[c('Date', 'Code')]),] 

write.csv(ac_overlap, "../data/overlap/acoustic_overlap_dataset_allstations.csv", row.names=FALSE)

########### Making it by week 


###### making it by day 

all_nestmonths <- ac_overlap %>%
  nest(data= -NewDate)

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))

for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(as.character(month), rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("day", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]

########### now have the count but need to make it by day 
summary_sharks$day <- as.Date(summary_sharks$day, "%Y-%m-%d")

count <- summary_sharks
count <- as.data.frame(count)

cols <- c("Var1", "Freq", "number_sharks")
colnames(count) <- cols
#count <- table(summary_sharks$day)
#count <- as.data.frame(count)

########## Filling in the missing dates so the data is continuous by the day 


ts <- seq.POSIXt(as.POSIXct("2014-01-05",'%y-%m-%d'), as.POSIXct("2019-04-26",'%y-%m-%d'), by="days")

ts <- format.POSIXct(ts,'%y-%m-%d')

df <- data.frame(Var1=ts)
df$Var1 <- substr(df$Var1, 0, 10)

df$Var1 <- as.Date(df$Var1, "%y-%m-%d")

######### Removing missing data periods 
DATE1<-as.Date("2016-06-28")  
DATE2<-as.Date("2017-06-03")

daterange <- interval(DATE1, DATE2)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))

date3 <- as.Date("2018-07-01")
date4 <- as.Date("2018-07-31")

daterange <- interval(date3, date4)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))

date5 <- as.Date("2018-01-01")
date6 <- as.Date("2018-01-31")

daterange <- interval(date5, date6)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))

count$Var1 <- as.Date(count$Var1, "%Y-%m-%d")

#data_with_missing_times <- full_join(count,df, by = "Var1")
data_with_missing_times <- merge(count,df, all = TRUE)

data_with_missing_times$Freq[is.na(data_with_missing_times$Freq)] <- 0
data_with_missing_times$number_sharks[is.na(data_with_missing_times$number_sharks)] <- 0


data_with_missing_times$day <- substr(data_with_missing_times$Var1, 9,10)
data_with_missing_times$month <- substr(data_with_missing_times$Var1, 6,7)
data_with_missing_times$year <- substr(data_with_missing_times$Var1, 1,4)
data_with_missing_times$monthyear <- substr(data_with_missing_times$Var1, 1, 7)


write.csv(data_with_missing_times, "../data/overlap/all_days/days_acoustic_overlap_dataset_allstations.csv", row.names=FALSE)


####################################################################################################################

####### OVERLAP WITH ACOUSTIC EXCLUDING LATER STATIONS POST 2016 

######################################################################################################################

ac_BPV <- read.csv("acoustic_BPV_dataset.csv")


r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
ac_BPV$distance<-distHaversine(ac_BPV[,3:4], ac_BPV[,7:8], r=r.km)
ac_overlap <- ac_BPV[ac_BPV$distance<sep.km,]

####### VERY IMPORTANT NOT TO INCLUDE REPEATING DATA

ac_overlap <- as.data.frame(ac_overlap)

####### have decided to go with this method of subsetting as it also removes multiple station detections 
ac_overlap <- ac_overlap[!duplicated(ac_overlap[c('Date', 'Code')]),] 

stations <- read.table("/Users/emmadeeks/Dropbox/Overlap_data/Station_attributes_full.csv", header = TRUE, sep = ",", dec = ".")
stations <- stations[,-4]
stations <- stations[,-4]
stations <- stations[,-5]
stations <- stations[,-5]

# selecting station IDs from 2016
year_2016 <- stations[stations$Year.Installed == '1',]
station_id <- year_2016[!duplicated(year_2016['station']),] 
station_id <- station_id$station

# subsetting initial dataframe of overlap by the stations only installed in 2016 
try <- ac_overlap[ac_overlap$station %in% station_id,]

######## writing a dataframe with overlap of just stations but no additional days added 
write.csv(try, "../data/overlap/2016_stations_acoustic_overlap_dataset.csv", row.names=FALSE)


##### now adding days 

all_nestmonths <- try %>%
  nest(data= -NewDate)

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))

for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(as.character(month), rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("day", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]

########### now have the count but need to make it by day 
summary_sharks$day <- as.Date(summary_sharks$day, "%Y-%m-%d")

count <- summary_sharks
count <- as.data.frame(count)

cols <- c("Var1", "Freq", "number_sharks")
colnames(count) <- cols
#count <- table(summary_sharks$day)
#count <- as.data.frame(count)

########## Filling in the missing dates so the data is continuous by the day 


ts <- seq.POSIXt(as.POSIXct("2014-01-05",'%y-%m-%d'), as.POSIXct("2019-04-26",'%y-%m-%d'), by="days")

ts <- format.POSIXct(ts,'%y-%m-%d')

df <- data.frame(Var1=ts)
df$Var1 <- substr(df$Var1, 0, 10)

df$Var1 <- as.Date(df$Var1, "%y-%m-%d")

######### Removing missing data periods 
DATE1<-as.Date("2016-06-28")  
DATE2<-as.Date("2017-06-03")

daterange <- interval(DATE1, DATE2)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))

date3 <- as.Date("2018-07-01")
date4 <- as.Date("2018-07-31")

daterange <- interval(date3, date4)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))

date5 <- as.Date("2018-01-01")
date6 <- as.Date("2018-01-31")

daterange <- interval(date5, date6)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))

count$Var1 <- as.Date(count$Var1, "%Y-%m-%d")

#data_with_missing_times <- full_join(count,df, by = "Var1")
data_with_missing_times <- merge(count,df, all = TRUE)

data_with_missing_times$Freq[is.na(data_with_missing_times$Freq)] <- 0
data_with_missing_times$number_sharks[is.na(data_with_missing_times$number_sharks)] <- 0


data_with_missing_times$day <- substr(data_with_missing_times$Var1, 9,10)
data_with_missing_times$month <- substr(data_with_missing_times$Var1, 6,7)
data_with_missing_times$year <- substr(data_with_missing_times$Var1, 1,4)
data_with_missing_times$monthyear <- substr(data_with_missing_times$Var1, 1, 7)


write.csv(data_with_missing_times, "../data/overlap/all_days/2016_stations_days_acoustic_overlap_dataset.csv", row.names=FALSE)




###### OVERLAP WITH SATELLITE DATA 


sat_BPV <- read.csv("satellite_BPV_dataset.csv")


r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
sat_BPV$distance<-distHaversine(sat_BPV[,3:4], sat_BPV[,7:8], r=r.km)
sat_overlap <- sat_BPV[sat_BPV$distance<sep.km,]

####### VERY IMPORTANT NOT TO INCLUDE REPEATING DATA

sat_overlap <- as.data.frame(sat_overlap)

####### have decided to go with this method of subsetting as it also removes multiple station detections 
sat_overlap <- sat_overlap[!duplicated(sat_overlap[c('Date', 'Code')]),] 

write.csv(sat_overlap, "../data/overlap/satellite_overlap_dataset.csv", row.names=FALSE)


all_nestmonths <- sat_overlap %>%
  nest(data= -NewDate)

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))

for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(as.character(month), rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("day", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]


write.csv(summary_sharks, "../data/overlap/all_days/summary_satellite_overlap_dataset.csv", row.names=FALSE)

