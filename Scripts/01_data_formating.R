#' ---
#' title: "01 albopictus OW inital"
#' author: "Katie Susong"
#' date: "15th September 2020"
#' ---

# set wd
setwd("~/Documents/CBS PhD/albopictus OW")

# Function Source
source('Scripts/01 Functions.R')

#libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(knitr))
shhh(library(tidyr))
shhh(library(plyr))

# import the 3 data file 
OW <- read.csv("Data/OWALL_030520.csv")
OWA <- read.csv("Data/OWALL_A030520.csv")
OWAVG <- read.csv("Data/OWALL_AVG030520.csv")

#' 
#' ------------------------------------------------------------------------------
#' 
#'### Formating 
#'
#'#### Date and Time 
#'
#' The date and time are not in the most useful format so I will update these  
#'     - The date will just be structured as a date without any format change    
#'     - I am going to change the time to 24H time    
#'     - The timestamps also do not lineup so that needs to be fixed  
#'     - I am also going to add a DateTime object that is rounded to the minute  
#'   

OW    <- DateTimeForm(OW)  
OWA   <- DateTimeForm(OWA)
OWAVG <- DateTimeForm(OWAVG)


#'#### Location repeats
#'
#'Because of the variation in each repeat it will be useful to keep them seperate with the 
#'possiblity to combine them. I will fallow the methid in BT_exploration.Rmd to reassign the 
#'"Number" to a "repeat". The number variable may not be important for later modeling. The significance 
#'of location can be determined with out including the "Number". "Repeat" will not be used in modeling 
#'
#'A = 1, 4, 7, 10, 13  
#'B = 2, 5, 8, 11, 14  
#'C = 3, 6, 9, 12, 15  


OW    <- RepeatABC(OW)
# this df has a differnt set of **Number** with an "A" after the number this will need
# to be changed before it can be used 
OWA$NumberA <- OWA$Number
OWA$Number <- as.numeric(as.character(revalue(OWA$Number, c("1A" = "1","2A"="2", "3A" = "3", "4A" = "4","5A"= "5",
                                                            "6A"= "6","7A"= "7", "8A" = "8", "9A"= "9", "10A"= "10",
                                                            "11A" = "11", "12A"= "12", "13A"="13", "14A" ="14", "15A"="15"))))
OWA   <- RepeatABC(OWA)
OWAVG <- RepeatABC(OWAVG)


#'### Wide data files 
#'
#' To address the temperature difference between Ambient and Tire I am
#' pivoting the data frame to a wider format, observations will 
#' be collected by the DateTime and stored in a new data table
#' 

## OW
OW.wide <- OW %>%
  pivot_wider(names_from = Type, values_from = Value) #pivot the table to a wide format
OW.wide$Diff <- OW.wide$Tire -OW.wide$Ambient  # create new data column with the difference between ambient and tire temperature 

## OWA
OWA.wide <- OWA %>%
  pivot_wider(names_from = Type, values_from = Value)
OWA.wide$Diff <-   OWA.wide$Tire - OWA.wide$Ambient

## OWAVG
OWAVG.wide <- OWAVG %>%
  pivot_wider(names_from = Type, values_from = Avg_Value)
OWAVG.wide$Diff <- OWAVG.wide$Tire - OWAVG.wide$Ambient 

#' 
#' -------------------------------------------------------------------------
#' 
#' ### Store new Data Frames
#' 
#' The newly formated tables and the wide data tables will be saved in the data folder
#' 

#write.csv(OW, "Data/OWALL.f_KMS_092220.csv")
#write.csv(OWA, "Data/OWALLA.f_KMS_092220.csv")
#write.csv(OWAVG, "Data/OWALLAVG.f_KMS_092220.csv")

#write.csv(OW.wide, "Data/OWALL.wide_KMS_092220.csv")
#write.csv(OWA.wide, "Data/OWALLA.wide_KMS_092220.csv")
#write.csv(OWAVG.wide, "Data/OWALLAVG.wide_KMS_092220.csv")


        



