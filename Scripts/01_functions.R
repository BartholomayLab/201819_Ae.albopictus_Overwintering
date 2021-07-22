#' ---
#' title: "FUNCTIONS: 01 albopictus OW inital"
#' author: "Katie Susong"
#' date: "22nd September 2020"
#' ---

shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(lubridate))

#'#### Function 1: DateTimeForm()  
#' 
#' *This function does serveral things:*   
#'   1. Format the date as a date  
#'   2. Format the time as 24H time  
#'   3. Remove the AMPM column  
#'   4. Combine Date and Time as a single column    
#'   5. Round the time in the DateTime Function to the 3rd hour   
#'   6. Remove the Date and Time colums, (they will mess stuff up later)  
#'   7. Save the DF   
#'   
#' *Parameters:*  
#'   DF: Data frame which includes columns **Date**, **Time**, and **AMPM**
#'

DateTimeForm <- function(DF) {
  DF$Date <- as.Date(DF$Date, format="%d-%b-%y")                 # Date
  DF$Time <- format(strptime(paste(DF$Time, DF$AMPM,sep = " "), 
                             "%I:%M %p"),"%H:%M")                # 24H time
  DF <- subset(DF, select = -c(AMPM))                            # remove AMPM 
  DF$DateTime <- with(DF, ymd(Date) + hm(Time))                  # combine to single object
  DF$DateTime <- as.POSIXct(ceiling_date(DF$DateTime, "3 hour")) # round time
  DF <- subset(DF, select = -c(Date,Time,Unit))                  # remove date and time which are redundnet
  return(DF)
  }


#'#### Function 2: RepeatABC()
#'
#'*This Function:*  
#'   Turns the "Number" variable which refers to the repeats at each site and are labled 1-15 
#'   to a "ABC" varaible that only has three levels, **A, B, C**. 
#'   
#'    A = 1, 4, 7, 10, 13  
#'    B = 2, 5, 8, 11, 14  
#'    C = 3, 6, 9, 12, 15  

#'   
#'*Parameters*  
#'  DF: Data Frame which includes the cloumn with values 1-15 in the **second** column 
#'  

RepeatABC <- function(DF){
  DF$ABC <- ifelse(DF[2] == 1 |DF[2]==4 | DF[2]==7 | DF[2]==10 | DF[2]==13, "A",  
                   ifelse(DF[2] == 2 |DF[2]==5 | DF[2]==8 | DF[2]==11 | DF[2]==14, "B", 
                          "C")) # group and create new variable 
  return(DF)
}






