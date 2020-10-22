rm(list=ls()) #Clear global environment 

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

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/Publication/data") #go to the data directory 


ac_overlap <- read.csv("../data/overlap/acoustic_overlap_dataset_allstations.csv")

########### Making it by week 
ac_overlap$NewDate <- as.Date(ac_overlap$NewDate, "%Y-%m-%d")

count <- ac_overlap
count <- as.data.frame(count)

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

col <- ("NewDate")
colnames(df) <- col

count$Var1 <- as.Date(count$Var1, "%Y-%m-%d")

#data_with_missing_times <- full_join(count,df, by = "Var1")
data_with_missing_times <- merge(count,df, all = TRUE, by = "NewDate")

data_with_missing_times <- as.data.frame(data_with_missing_times)

data_with_missing_times$NewDate <- as.Date(data_with_missing_times$NewDate, "%Y-%m-%d")

########### grouping by week 
data_with_missing_times <- data_with_missing_times %>% 
  mutate(week = cut.Date(NewDate, breaks = "1 week", labels = FALSE)) %>% 
  arrange(NewDate)

###### making it by day 

all_nestmonths <- data_with_missing_times %>%
  nest(data= -week)

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 4))

for (i in 1:length(all_nestmonths$week)){
  monthdata <- all_nestmonths$data[[i]]
  Date <- monthdata$NewDate[[1]]
  month <- all_nestmonths$week[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(month, rows, no_sharks, as.character(Date))
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("week", "count", "number_sharks", "date")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]

write.csv(summary_sharks, "../data/overlap/all_weeks/weeks_acoustic_summary_all_stations", row.names=FALSE)

########### weeks for 

post_ac_overlap <- read.csv("overlap/2016_stations_acoustic_overlap_dataset.csv")


########### Making it by week 
post_ac_overlap$NewDate <- as.Date(post_ac_overlap$NewDate, "%Y-%m-%d")

count <- post_ac_overlap
count <- as.data.frame(count)

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

col <- ("NewDate")
colnames(df) <- col

#count$Var1 <- as.Date(count$Var1, "%Y-%m-%d")

#data_with_missing_times <- full_join(count,df, by = "Var1")
data_with_missing_times <- merge(count,df, all = TRUE, by = "NewDate")

data_with_missing_times <- as.data.frame(data_with_missing_times)

data_with_missing_times$NewDate <- as.Date(data_with_missing_times$NewDate, "%Y-%m-%d")

########### grouping by week 
data_with_missing_times <- data_with_missing_times %>% 
  mutate(week = cut.Date(NewDate, breaks = "1 week", labels = FALSE)) %>% 
  arrange(NewDate)

###### making it by day 

all_nestmonths <- data_with_missing_times %>%
  nest(data= -week)

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 4))

for (i in 1:length(all_nestmonths$week)){
  monthdata <- all_nestmonths$data[[i]]
  Date <- monthdata$NewDate[[1]]
  month <- all_nestmonths$week[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(month, rows, no_sharks, as.character(Date))
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("week", "count", "number_sharks", "date")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]

write.csv(summary_sharks, "overlap/all_weeks/2016_stations_weeks_acoustic_summary", row.names=FALSE)


