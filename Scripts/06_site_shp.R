#' ---
#' title: "06 OW albopictus- Create site .shp"
#' author: "Katie Susong"
#' date: "12th October 2020"
#' ---

# set wd
setwd("~/Documents/CBS_PhD/albopictus_OW/")

# libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(sp))
shhh(library(rgdal))

# import site file
site.loc <- read.csv("Data/site_location.csv")

#'### Make .shp File parameters 
#'
#box
bbox <- matrix(c(-130.516666666661, 58.2333333333310 ,-62.2499999999975, 24.0999999999990), 
               nrow=2, ncol =2,
               byrow = TRUE,
               dimnames = list(c("x","y"),c("min", "max")))
# define projection
proj4string <- CRS("+proj=longlat +datum=WGS84")
# coord system 
coord.df <- data.frame("X" = site.loc$Lon, "Y"= site.loc$Lat)
#variables
data.df <- data.frame("Number" = site.loc$Site, 
                      "Name" = site.loc$Name, 
                      "State"= site.loc$State, 
                      "City" = site.loc$City)

site.points <- SpatialPointsDataFrame(coords = coord.df,
                                           data = data.df,
                                           proj4string = proj4string,
                                           bbox = bbox) 

writeOGR(site.points, "Data/site_points", layer = "site_points1", driver = "ESRI Shapefile")
