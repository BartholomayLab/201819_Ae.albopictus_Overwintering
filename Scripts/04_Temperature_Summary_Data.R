#' ---
#' title: "04 OW albopictus Generate Summary Data"
#' author: "Katie Susong"
#' date: "12th October 2020"
#' ---

# set wd
setwd("~/Documents/CBS_PhD/albopictus_OW/")

# libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(lubridate))
shhh(library(dplyr))

# import temperature data frame- WIDE
OWw    <- read.csv("Data/OWALL.wide_KMS_092220.csv")
#format corrections
OWw$DateTime <- ymd_hms(OWw$DateTime)

# import temperature data frame - TIDY
OW <- read.csv("Data/OWALL.f_KMS_092220.csv")
#format corrections
OW$DateTime <- ymd_hms(OW$DateTime)

# import survival data frame
hatch <- read.csv("Data/OWhatchALL.csv")

#'### Process survival data
#'
#' Add percent survival and change Location variable's names
#' 

# add survival percent
hatch$PerSur <- round(hatch$Total.Larvae  / hatch$Egg.Count *100, 3)
# reanme the location variable 
names(hatch)[names(hatch)=="Location.Tire"] <- "Number"
# fix format
hatch$Number <- as.factor(hatch$Number)


#'### Process temperature data
#'
#' There are extream temperature values that are  because of the ibuttons reading temperatures
#' before being set in place this ewill be remove by removing all dates before 11-Nov_2020
#' 

OW <- filter(OW, DateTime > "2018-11-13 21:00:00")
OWw <- filter(OWw, DateTime > "2018-11-13 21:00:00")

#'### Build Summary dataframe  
#'
#' Fliter by: species and dipuase  
#' Extract: Strip number, percent survival
#' 

# create summmary list 
AA.summary <- subset(hatch[hatch$Number != "Test Hatch",], select = -c(H1,H2,H3))
AA.summary$Number <- as.factor(AA.summary$Number)
#' The dataframe now includes the strip number, AA.summarypause, species, tire.location, egg count, 
#' total larve chatched, percent hatched
#' 
#' I will begin adding the summary values to the summary table
#' 
#'#### January Mean - Tire and Ambient
#'

#Tt is a temperary DF that will be re-written for each summary
Tt<-OWw %>% 
  filter(DateTime > "2018-12-31 21:00:00", DateTime < "2019-02-01 00:00:00") %>%  # filter to JAN
  group_by(Number) %>%
  dplyr::summarise(JANmeanT = mean(Tire),JANmeanA = mean(Ambient))                # calcualte mean 
# add the control temperatures 
Tt<- rbind(Tt,c("Control 4", 4,4)) 
Tt<- rbind(Tt,c("Control 27", 27,27))
#merge with summary DF
AA.summary <- merge(AA.summary, Tt,"Number", 
                    incomparables = c("Test Hatch"))
# correct format
AA.summary$JANmeanT <- round( as.numeric(AA.summary$JANmeanT), 2)
AA.summary$JANmeanA <- round( as.numeric(AA.summary$JANmeanA), 2)


#'#### January Mean - Difference
#'
#' JANmeanTire - JANmeanAmbient 
#' 
#' Positive number = the the Tire is warmer than ambient temperature 

AA.summary$JANmeanD <- -(abs(AA.summary$JANmeanT) - abs(AA.summary$JANmeanA))

#'#### Winter Mean Temperature - Tire and Ambient
#'
#' Winter is defined as December, January and Febuary.

Tt<-OWw %>% 
  filter(DateTime > "2018-11-30 21:00:00", DateTime < "2019-03-01 00:00:00") %>%  # filter to DJF
  group_by(Number) %>%
  dplyr::summarise(DJFmeanT = mean(Tire),DJFmeanA = mean(Ambient))                # calcualte mean 
# add the control temperatures 
Tt<- rbind(Tt,c("Control 4", 4,4)) 
Tt<- rbind(Tt,c("Control 27", 27,27))
#merge with summary DF
AA.summary <- merge(AA.summary, Tt,"Number")
# correct format
AA.summary$DJFmeanT <- round (as.numeric(AA.summary$DJFmeanT), 2)
AA.summary$DJFmeanA <- round( as.numeric(AA.summary$DJFmeanA), 2)

#'#### Winter Mean - Difference
#'
#' DJFmeanTire - DJFmeanAmbient 
#' 
#' Positive number = the the Tire is warmer than ambient temperature 

AA.summary$DJFmeanD <- AA.summary$DJFmeanT - AA.summary$DJFmeanA

#'#### Min Temp - Tire
#'
#'
Tt <- OWw %>%
  group_by(Number) %>%                                  # group by location
  summarise(MinT = min(Tire, na.rm = T), 
                       MinA =min(Ambient, na.rm = T))   # min temperature

# add the control temperatures 
Tt<- rbind(Tt,c("Control 4", 4,4)) 
Tt<- rbind(Tt,c("Control 27", 27,27))
#merge with summary DF
AA.summary <- merge(AA.summary, Tt,"Number")
# correct format
AA.summary$MinT <- round (as.numeric(AA.summary$MinT), 2)
AA.summary$MinA <- round (as.numeric(AA.summary$MinA), 2) 


#'#### Difference in Min Temperature 
#'
#' Same as all the other differences calculated, positive number indicates that
#' that tire is warmer than the ambient temperature

AA.summary$MinD <- AA.summary$MinT - AA.summary$MinA 


#'#### Date of First Frost - Tire
#' 
#'

Tt <- OWw %>%
  group_by(Number) %>%               # group by location
  filter(Tire < 0) %>%               # filter to temperatures below frost temperature
  summarise(FFrostT = min(DateTime)) # Find the earliest date 

# add the control temperatures 
Tt<- rbind(Tt,c("Control 4", NA)) 
Tt<- rbind(Tt,c("Control 27", NA))
#merge with summary DF
AA.summary <- merge(AA.summary, Tt,"Number")

#'#### Date of First Frost - Ambient
#' 
#'

Tt <- OWw %>% 
  group_by(Number) %>%               # group by location
  filter(Ambient < 0) %>%            # filter to temperatures below frost temperature
  summarise(FFrostA = min(DateTime)) # Find the earliest date 

# add the control temperatures 
Tt<- rbind(Tt,c("Control 4", NA)) 
Tt<- rbind(Tt,c("Control 27", NA))
#merge with summary DF
AA.summary <- merge(AA.summary, Tt,"Number")

#'#### Number of Days below -12 - Tire
#'
#' Given that lab experiments have found that there is a critical 
#' threshold at -12C where that eggs die.
#' 

OWw$Date <- date(OWw$DateTime)
Tt <- OWw %>%
  group_by(Number) %>%
  filter(Tire < -11.5) %>%
  summarise( DaysB12T = n_distinct(Date))

# add the control temperatures 
Tt<- rbind(Tt,c("Control 4", NA)) 
Tt<- rbind(Tt,c("Control 27", NA))
#merge with summary DF
AA.summary <- merge(AA.summary, Tt,"Number")

#'#### Number of Days below -12 - Ambient
#'
#' Given that lab experiments have found that there is a critical 
#' threshold at -12C where that eggs die.
#' 

Tt <- OWw %>%
  group_by(Number) %>%
  filter(Ambient < -11.5) %>%
  summarise(DaysB12A = n_distinct(Date))

# add the control temperatures 
Tt<- rbind(Tt,c("Control 4", NA)) 
Tt<- rbind(Tt,c("Control 27", NA))
#merge with summary DF
AA.summary <- merge(AA.summary, Tt,"Number")



# correct format
AA.summary$DaysB12T <- as.numeric(AA.summary$DaysB12T)
AA.summary$DaysB12A <- as.numeric(AA.summary$DaysB12A)

#'#### Consecutive hours below Zero -Tire
#'

# empty DF
ColdDays <- data.frame("Number"= rep(NA,15), "DaysB0conT" = rep(NA,15))
for (x in 1:15) {
  Tt <- OWw %>%
    filter (Number == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Tire < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  rBelow <- r$lengths[c(TRUE,FALSE)]            # extract only T in a row
  MaxBelow <-round(max(rBelow) * 3 , 0)         # convert 3 hour blocks to hours, round
  ColdDays[x,] <- c(x, MaxBelow)                # add to DF
}

# add the control temperatures 
ColdDays<- rbind(ColdDays,c("Control 4", 0)) 
ColdDays<- rbind(ColdDays,c("Control 27", 0))
#merge with summary DF
AA.summary <- merge(AA.summary, ColdDays,"Number")


#'#### Consecutive hours below Zero -Ambient
#'

# empty DF
ColdDays <- data.frame("Number"= rep(NA,15), "DaysB0conA" = rep(NA,15))
for (x in 1:15) {
  Tt <- OWw %>%
    filter (Number == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Ambient < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  rBelow <- r$lengths[c(TRUE,FALSE)]            # extract only T in a row
  MaxBelow <-round(max(rBelow) * 3 , 0)         # convert 3 hour blocks to days, round
  ColdDays[x,] <- c(x, MaxBelow)                # add to DF
}

# add the control temperatures 
ColdDays<- rbind(ColdDays,c("Control 4", 0)) 
ColdDays<- rbind(ColdDays,c("Control 27", 0))
#merge with summary DF
AA.summary <- merge(AA.summary, ColdDays,"Number")

#'#### Consecutive hours below -12 - Tire
#'

# empty DF
ColdDays <- data.frame("Number"= rep(NA,15), "HrsB12conT" = rep(NA,15))
for (x in 1:15) {
  Tt <- OWw %>%
    filter (Number == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Tire < -12, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  rBelow <- r$lengths[c(FALSE,TRUE)]            # extract only T in a row
  MaxBelow <-round(max(rBelow) * 3 , 0)         # convert 3 hour blocks to days, round
  ColdDays[x,] <- c(x, MaxBelow)                # add to DF
}

# add the control temperatures 
ColdDays<- rbind(ColdDays,c("Control 4", 0)) 
ColdDays<- rbind(ColdDays,c("Control 27", 0))
#merge with summary DF
AA.summary <- merge(AA.summary, ColdDays,"Number")

#'#### Consecutive hours below -12 - Ambient
#'

# empty DF
ColdDays <- data.frame("Number"= rep(NA,15), "HrsB12conA" = rep(NA,15))
for (x in 1:15) {
  Tt <- OWw %>%
    filter (Number == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Ambient < -12, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  rBelow <- r$lengths[c(FALSE,TRUE)]            # extract only T in a row
  MaxBelow <-round(max(rBelow) * 3 , 0)         # convert 3 hour blocks to hours, round
  ColdDays[x,] <- c(x, MaxBelow)                # add to DF
}

# add the control temperatures 
ColdDays<- rbind(ColdDays,c("Control 4", 0)) 
ColdDays<- rbind(ColdDays,c("Control 27", 0))
#merge with summary DF
AA.summary <- merge(AA.summary, ColdDays,"Number")


#'#### Freze- Thaw Events - Tire
#'

# empty DF
FreezeThaw <- data.frame("Number"= rep(NA,15), "FreThaT" = rep(NA,15))
for (x in 1:15) {
  Tt <- OWw %>%
    filter (Number == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Tire < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  events <- as.numeric(length(r$lengths))
  FreezeThaw[x,] <- c(x, events)
  }

# add the control temperatures 
FreezeThaw<- rbind(FreezeThaw,c("Control 4", 0)) 
FreezeThaw<- rbind(FreezeThaw,c("Control 27", 0))
#merge with summary DF
AA.summary <- merge(AA.summary, FreezeThaw,"Number")
  

#'#### Freze- Thaw Events - Ambient
#'

# empty DF
FreezeThaw <- data.frame("Number"= rep(NA,15), "FreThaA" = rep(NA,15))
for (x in 1:15) {
  Tt <- OWw %>%
    filter (Number == x)                        # filter by site number
  Tt$below0 <- ifelse(Tt$Ambient < 0, TRUE, FALSE) # create bool. variable
  r <- rle(Tt$below0)                           # tally T/F in a row 
  events <- as.numeric(length(r$lengths))
  FreezeThaw[x,] <- c(x, events)
}

# add the control temperatures 
FreezeThaw<- rbind(FreezeThaw,c("Control 4", 0)) 
FreezeThaw<- rbind(FreezeThaw,c("Control 27", 0))
#merge with summary DF
AA.summary <- merge(AA.summary, FreezeThaw,"Number")


#'#### Add location(study site)
#'

# uses ifelse statement to turn the tire number into a location  
AA.summary$Location <- ifelse(AA.summary$Number == 1 |AA.summary$Number==2 | AA.summary$Number==3, "New Berlin",
                       ifelse(AA.summary$Number==4 | AA.summary$Number==5 | AA.summary$Number == 6, "SCCMAD",
                              ifelse(AA.summary$Number == 7 |AA.summary$Number==8 | AA.summary$Number==9, "Kankakee",
                                     ifelse( AA.summary$Number==10 | AA.summary$Number== 11 | AA.summary$Number== 12, "NWMAD", 
                                             ifelse(AA.summary$Number==13 | AA.summary$Number==14 | AA.summary$Number==15,"Champaign-Urbana",
                                                    ifelse(AA.summary$Number=="Control 4", "Control4", "Control27"))))))
AA.summary$Location <- as.factor(AA.summary$Location)




# Write out summary file with temperature data 

## write.csv(AA.summary, "Data/OW_summary_12012021.csv")
