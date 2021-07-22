#' ---
#' title: "02 OW albopictus Temperature Data Cleaning"
#' author: "Katie Susong"
#' date: "23nd September 2020"
#' ---

# set wd
setwd("~/Documents/CBS PhD/albopictus OW")

#libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(dplyr))
shhh(library(tidyr))
shhh(library(ggplot2))
shhh(library(lubridate))

#import data frames- WIDE
OWw    <- read.csv("Data/OWALL.wide_KMS_092220.csv")
#format corrections
OWw$DateTime <- ymd_hms(OWw$DateTime)


# import data frames - TIDY
OW <- read.csv("Data/OWALL.f_KMS_092220.csv")
#format corrections
OW$DateTime <- ymd_hms(OW$DateTime)

# add an extra wide df
OW <- subset(OW, select =-c(X))
OWxw <- pivot_wider(subset(OW, select = -c(Number)),names_from = c(Type, ABC),values_from = Value)

#'## Reviewing Site for Anomalies
#'
#'As part of the data cleaning process I will visually review that data in each site to 
#'determine if there are any apparent anomalies that need to be explained or coreected
#'
#'#### Time block Temperture Readings
#'
OW %>%
  filter(Location == "Champaign-Urbana") %>%
  ggplot(aes(DateTime, Value))+
  geom_line()+
  facet_grid(ABC~Type) +
  ylab ( "Temperature (C)") +
  xlab( "Date") +
  ggtitle("Champaign-Urbana")
#' The **ambient** and **tire** are fairly consistent within themself. **TireA** shows some
#' much warmer spring days and some greater variation. The FEB cold drop was also visable in both 
#' the **tire** and the **ambient**
#' 
OW %>%
  filter(Location == "Kankakee") %>%
  ggplot(aes(DateTime, Value))+
  geom_line()+
  facet_grid(ABC~Type) +
  ylab ( "Temperature (C)") +
  xlab( "Date") +
  ggtitle("Kankakee")
#' **AmbientA** started much warme than **B** or **C**. **TireC** had greater variation in APR/MAY.
#' the FEB cold drop is visable 
#' 
OW %>%
  filter(Location == "New Berlin") %>%
  ggplot(aes(DateTime, Value))+
  geom_line()+
  facet_grid(ABC~Type) +
  ylab ( "Temperature (C)") +
  xlab( "Date") +
  ggtitle("New Berlin")
#' The **ambient** temperture looks very consitent though out the site. There cold drop that is
#' seen in FEB was reflected in the A and C **tire** replicates. However this is not true in the 
#' B **tire** replicate. I believe this this the replicate that has survival in it. Determing why 
#' this replocate was more warmer could be a key finding. That information may not be possible. 
#' Could the temperature monitoring be repeated this winter (with or with/o eggs) to see if this 
#' is repeated? 
#'
OW %>%
  filter(Location == "NWMAD") %>%
  ggplot(aes(DateTime, Value))+
  geom_line()+
  facet_grid(ABC~Type) +
  ylab ( "Temperature (C)") +
  xlab( "Date") +
  ggtitle("NWMAD")
#' **AmbientC** starts much warmer. The FEB cold drop is only visable in the **ambient** and not in
#' the **tire** 
#' 
OW %>%
  filter(Location == "SCCMAD") %>%
  ggplot(aes(DateTime, Value))+
  geom_line()+
  facet_grid(ABC~Type) +
  ylab ( "Temperature (C)") +
  xlab( "Date") +
  ggtitle("SCCMAD")
#' The **ambient** and **tire** are fairly consistent within themself. The **Tires** are more 
#' consistnet with each other. The FEB cold drop is still viable.
#'
#'## Variable correlation
#'
#' Using an extra wide version of the data set I will compare te **ambient** and **tire** temperatures of 
#' each site at all of the replicates
#' 
#'
#'#### All Locations
#'
pairs(OWw)
#' The **ambient** and **tire ** temperture seem to follow a similar wide distribution there is a 
#' spike in the **difference** temperture around FEB that matches the cold drop and the warm 
#' tire in New Berlin
#' 
#'#### Champaing-Urbana
#'
pairs(filter(OWxw, Location == "Champaign-Urbana"))
#'There are some outliners in this location. When comparing the **ambient** and **tire**
#'tempertures at each replicate there are outliners during the cold snap or during 
#'the spring
#'
#'#### Kankakee
#'
pairs(filter(OWxw, Location == "Kankakee"))
#'There are some outliners in this location. When comparing the **ambient** and **tire**
#'tempertures at each replicate there are outliners during the cold snap or during 
#'the spring
#'
#'#### New Berlin
#'
pairs(filter(OWxw, Location == "New Berlin"))
#' As expected given the warm **tireB** is not linear. **tireB** does not correlate with
#' other tires. The **ambeint** temperatures are correlated.
#' 
#'#### NWMAD
#'
pairs(filter(OWxw, Location == "NWMAD"))
#'All of the **ambeint** and **tire** temperatures are correlated with each other.
#'
#'#### SCCMAD
#'
pairs(filter(OWxw, Location == "SCCMAD"))
#' All of the **ambient** tempertures correlate. The **tire** temperture and is a wider
#' regression but still correlate. The correlation of **tireC** and **ambientC** is non-
#' linear in the spring.
#'  








