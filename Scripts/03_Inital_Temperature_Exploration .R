#' ---
#' title: "03 OW albopictus Temperature Data Exploration"
#' author: "Katie Susong"
#' date: "22nd September 2020"
#' ---

# set wd
setwd("~/Documents/CBS PhD/albopictus OW/")

#libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(ggplot2))
shhh(library(lubridate))
shhh(library(dplyr))

#import data frames- WIDE
OWw    <- read.csv("Data/OWALL.wide_KMS_092220.csv")
#format corrections
OWw$DateTime <- ymd_hms(OWw$DateTime)



# import data frames - TIDY
OW <- read.csv("Data/OWALL.f_KMS_092220.csv")
#format corrections
OW$DateTime <- ymd_hms(OW$DateTime)

#'##### 1. What is the temperature difference between Ambient and Tire each day
#' 
#' This differnce was added to the wide table during formating. Now I will graph each 
#' of the data frames to show temperature difference (Tire - Ambient) so that when the 
#' tire temperature is warmer the difference is positive
#' 
ggplot(OWw,aes(DateTime, Diff)) +
  geom_line(aes(col=ABC), linetype = 2)+
  facet_grid(~Location) +
  theme_bw() + 
  ggtitle("The Diffence in Ambient and Tire Temperature")
# filter to winter months 
OWw %>%
  filter(DateTime > "2018-12-01 00:00:00", DateTime < "2019-03-01 00:00:00") %>%
ggplot(aes(DateTime, Diff)) +
  geom_line(aes(col=ABC), linetype = 2)+
  facet_grid(~Location) +
  theme_bw() + 
  ggtitle("The Diffence in Ambient and Tire Temperature During Winter")
#  avg the replicates 
OWw %>%
  group_by(Location, DateTime) %>%
  summarise(DiffAvg = mean(Diff)) %>%
  ggplot(aes(DateTime, DiffAvg)) +
  geom_line()+
  facet_grid(~Location) +
  theme_bw() +
  ggtitle("The Average Diffence in Ambient and Tire Temperature")
#  avg the replicates and filter to winter months
OWw %>%
  group_by(Location, DateTime) %>%
  summarise(DiffAvg = mean(Diff)) %>%
  filter(DateTime > "2018-12-01 00:00:00", DateTime < "2019-03-01 00:00:00") %>%
  ggplot(aes(DateTime, DiffAvg)) +
  geom_line()+
  facet_grid(~Location) +
  theme_bw() +
  ggtitle("The Average Diffence in Ambient and Tire Temperature During Winter")
  
#'
#' * There are not clear repeating trends at any location or across the locations. 
#'   
#' * There is not any seasonal trend.   
#' 
#' * Early and late season warm tires could be due to sun.   
#' 
#' * The very late season shows uniform tempertures   
#' 
#' * **Champaign-Urban** has two similar replicates.   
#' 
#' * **New Berlin** rarely has colder tires but the warm spike in FEB is only 
#'     seen in replicate B. This replicate also seems to stay warmer for longer
#'     
#' * Further south locations (**Champaign-Urban**) have lover temperature differnce in the winter 
#'     months  
#'     
#' * In **New Berlin** there is less temperature swings in temperature differnce 
#'     in DEC and JAN. 
#'     
#' * **NWMAD** has the greatest differnce in temperature during the winter period
#' 


#'##### 2. How many observation were below -12C?
#'

# generate count by location:
OW$Below12 <- ifelse(OW$Value < -12, 1, 0) # create a 0/1 column for tempertures below -12
# pipe 
OW_below12 <- OW %>%
  group_by(Location, ABC, Type) %>% 
  # summary includes count of observations below zero, total observation and percent
  dplyr::summarise(Below12 = sum(Below12),N = n(), Perc = round(sum(Below12)/n()*100, 2))

#' Two replicats had a very low number of observations below -12C, **New Berlin** and **NWMAD**.
#' For each location the temperture was only below -12C for one day.
subset(OW, Location == "New Berlin" & ABC == "B" & Type  == "Tire" & Value < -12)
subset(OW, Location == "NWMAD" & ABC == "C" & Type  == "Tire" & Value < -12)

#'#### How many days were below -12C?
#'
OW$Date <- date(OW$DateTime)
OW_below12_DAY <- OW %>%
  group_by(Date, Type) %>%
  # summary includes count of observations below zero, total observation and percent
  dplyr::summarise(Below12 = sum(Below12),N = n(), Perc = round(sum(Below12)/n()*100, 2))

# filter OW_below12_DAY for days with observations below 12 in the tires 
(filter(OW_below12_DAY, Below12 >0, Type == "Tire"))
#'
#'  There were 25 day with at least 1 temperture observation below -12
#'  


