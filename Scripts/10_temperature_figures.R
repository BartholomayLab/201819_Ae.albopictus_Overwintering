#' ---
#' title: " 10 OW albopictus Temperature at site graphs "
#' author: "Katie Susong"
#' date: "07 January 2021"
#' ---


# set wd
setwd("~/Documents/CBS_PhD/albopictus_OW/")

#libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(ggplot2))
shhh(library(lubridate))
shhh(library(dplyr))

#import data frames- WIDE
OWw    <- read.csv("Data/OWALL.wide_KMS_092220.csv")
#format corrections
OWw$DateTime <- ymd_hms(OWw$DateTime)

#import data frames- TIDY
OW    <- read.csv("Data/OWALL.f_KMS_092220.csv")
#format corrections
OW$DateTime <- ymd_hms(OW$DateTime)
# change location order to be consistent 
OW$Location <- factor(OW$Location , levels=c("New Berlin", "NWMAD","SCCMAD" ,"Kankakee", "Champaign-Urbana"))



#'## Plot Temperature Profiles 
#'

#Create average temperature at sites df
OW.avg <- OW %>%
  group_by(Location, Type, DateTime) %>%
  summarise(mean = mean(Value, na.rm = T))
OW.avg <- as.data.frame(OW.avg)
OW.avg$DateTime <- ymd_hms(OW.avg$DateTime)

# time to mean temperature, line graph 
OW.avg %>%
  filter(DateTime >= "2018-12-01 00:00:00", DateTime <= "2019-03-31 24:00:00") %>%
  ggplot( aes(DateTime,mean, col = Type))+
  geom_line() +
  facet_wrap(~Location, ncol= 1) +
  geom_hline(yintercept = 0, linetype= "dashed") + 
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  ylab("Mean Site Temperature (??C)") +
  xlab("Date (ymd_h)") +
  theme_linedraw() +
  theme(legend.position = "none") +
  annotate("rect", xmin = as.POSIXct("2019/01/01 00:00:00"), xmax = as.POSIXct("2019/01/31 24:00:00"),
           ymin =-Inf, ymax = Inf , 
           alpha = .5)

#'## Temperarure min/max at sites
#'

OW.avg %>%
  group_by(Location, Type) %>%
  summarise(MinT = min(mean), MaxT = max(mean))

#'## Temperarue: Jan Mean 
#'

OW.avg %>%
  group_by(Location,Type) %>%
  filter(DateTime > "2018-12-31", DateTime < "2019-02-01") %>%
  summarise(JANmean = mean(mean))
           
           
           
           
           
           
           
           
           
           
