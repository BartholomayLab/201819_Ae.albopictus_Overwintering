#' ---
#' title: "Site Suvival Summary Table"
#' author: "Katie Susong"
#' date: "07th April 2021"
#' ---

# set wd
setwd("~/Documents/CBS_PhD/albopictus_OW")

# libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(ggplot2))
shhh(library(dplyr))
shhh(library(tidyr))
shhh(library(gridExtra))

#' Import Dataframe
#' 
summary <- read.csv("~/Documents/CBS_PhD/albopictus_OW/Data/OW_summary.csv")
# fix factor order
summary$Location <- factor(summary$Location , levels=c("New Berlin", "NWMAD","SCCMAD" ,"Kankakee", "Champaign-Urbana", "Control4","Control27"))

# Create AT/AA survival table
summary.sh <- data.frame("Site" = summary$Number, "Species" = summary$Species, "Diapause" = summary$Diapause, "PerSur" = summary$PerSur)
summary.w  <- pivot_wider(summary.sh, names_from = c(Species, Diapause), values_from =  PerSur, values_fn = list )
print(summary.w)