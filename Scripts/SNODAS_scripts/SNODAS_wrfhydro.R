#' ---
#' title: "?? OW albopictus- SNODAS snow depth collection method 2"
#' author: "Katie Susong"
#' date: "12th October 2020"
#' ---


# libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(rwrfhydro))
shhh(library(lubridate))
shhh(library(processNC))
shhh(library(raster))
shhh(library(tidyverse))
shhh(library(ncdf4))

# set path to directory 
snodasPath <- ("/Volumes/Seagate/SNODAS/studydates")

# collect all study dates 
startdate <- ymd("2018-11-02")
enddate <- ymd("2019-04-30")
# seq. of all dates
datesWanted <- seq(startdate, enddate, by = "days")

# function to add to snodas directory whilr also checking for existing dates and returning a summary 
UpdateSnodas <- function(POSIXct, outPath='.') {
  ## check if we already processed this date/POSIXct, if we didnt process, we'll
  ## download again and process it. (note we could have downloaded but not processed 
  ## so it might not be efficient). 
  file <- paste0(outPath, 'SNODAS_',format(POSIXct,'%Y%m%d'),'.nc')
  processed <- file.exists(file)
  if(processed) 
    return(data.frame(date=POSIXct, snodasGot=FALSE, ncdfFile=file))
  
  snodasGot <- GetSnodasDepthSweDate(POSIXct, outputDir=outPath)
  if(snodasGot) {
    snodasList <- ReadSnodasDepthSweDate(POSIXct, outputDir=outPath)
    ncdfFile <- PutSnodasNcdf(snodasList, outputDir=outPath)
  } else ncdfFile <- ''
  data.frame(date=POSIXct, snodasGot=snodasGot, ncdfFile=ncdfFile)
}

#add all study dates to folder (started: 14:26 2020-12-05, finished:18:45  averaging 1min per file , final files size: ~70gb)
update <- plyr::ldply(NamedList(datesWanted), UpdateSnodas, outPath=snodasPath)

# add coord system to file
files <- list.files(path = snodasPath,pattern=".nc", full.names=TRUE)
files <- PutSnodasCoordsNcdf()

# test Get snowDepth for one study location
GetSnodasPointTs(bDatePOSIXct = startdate, eDatePOSIXct= enddate, snodasDir = snodasPath, lat=43.00342, lon = -88.12294,
                 quiet = TRUE)
