#' ---
#' title: " 07 OW albopictus- Snow Insulation Data, "
#' author: "Katie Susong"
#' date: "16 December 2020"
#' ---

# set wd
setwd("~/Documents/CBS_PhD/albopictus_OW/")

#libraries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(dplyr))
shhh(library(lubridate))
shhh(library(tidyr))


#Import data 
SUM <- read.csv("Data/OW_summary_INPROG_04112020.csv") # current summary file
OW  <- read.csv("Data/OWALL.f_KMS_092220.csv")         # daily files 
SNO <- read.csv("Data/OWsitesnow.csv")                 # snow depth file 


# set day variable in the daily files ( exclude time stamp)
OW$Date <- date(OW$DateTime)
SNO$date <- ymd(SNO$date)

# filter dates to study limit
OW <- filter(OW, Date > "2018-11-13")
SNO <- filter(SNO, date > "2018-11-13")

### Daily temperature averages and min at sites ###
DIA <- OW %>%
  group_by(Type,Number,Date) %>%
  dplyr::summarise(MeanT = mean(Value), MinT = min(Value))

# fix date format
DIA$date <- as_date(DIA$Date)
DIA$site <- DIA$Number

# Widen table for better analysis
DIA <- DIA %>%
  pivot_wider(names_from = Type, values_from = c(MeanT, MinT)) #pivot the table to a wide format
#remove un-needed NA col
DIA <- subset(DIA, select = c(site, date, MeanT_Ambient, MeanT_Tire, MinT_Ambient, MinT_Tire)) 
# create new data columns with the difference between ambient and tire temperature 
DIA$MeanD <- DIA$MeanT_Tire -DIA$MeanT_Ambient # warmer tire = positive number
DIA$MinD <- DIA$MinT_Tire -DIA$MinT_Ambient # warmer tire = positive number

### Date daily Snow depth ###
DIA <- merge(DIA, SNO, by=c("site","date"))

# snow depth factors 
DIA$snow_FAC <- cut(DIA$snow_depth, breaks = c(-1, 0,195,400), labels = c("none", "lesser", "greater"))
DIA$snow_FAC10 <- cut(DIA$snow_depth, breaks = c(-1, 0,100,200,300,400), labels = c("none", ">100", ">200", ">300", ">400"))

# add locations variable 
DIA$Location <- ifelse(DIA$site == 1 |DIA$site==2 | DIA$site==3, "New Berlin",
                       ifelse(DIA$site==4 | DIA$site==5 | DIA$site == 6, "SCCMAD",
                              ifelse(DIA$site == 7 |DIA$site==8 | DIA$site==9, "Kankakee",
                                     ifelse( DIA$site==10 | DIA$site== 11 | DIA$site== 12, "NWMAD", "Champaign-Urbana"))))
DIA$Location <- as.factor(DIA$Location)

## write.csv(DIA, "Data/SnowSite_07012021_kms.csv")



