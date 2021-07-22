#' ---
#' title: " 08 OW albopictus- Survival Analysis, "
#' author: "Katie Susong"
#' date: "05 January 2021"
#' ---

# set wd
setwd("~/Documents/CBS_PhD/albopictus_OW")

# libaries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(ggplot2))
shhh(library(dplyr))
shhh(library(brms))
shhh(library(GGally))


# Import Dataframe
SUR <- read.csv("Data/OW_summary_12012021.csv")


#check strucute 
str(SUR)
# change Strip to a factor
SUR$Strip <- as.factor(SUR$Strip)

# make surival y/n variable
SUR$SurYN <- as.factor(ifelse(SUR$PerSur > 0 , "Y", "N"))

#reorder location
SUR$Location <- factor(SUR$Location , levels=c("New Berlin", "NWMAD","SCCMAD" ,"Kankakee", "Champaign-Urbana", "Control4", "Control27"))

# filer to AA & Diapausing survival

SUR_AAD <- SUR %>%
  filter(Species == "AA", Diapause == "Y")
# make PerSur a proportion for beta analysis 
SUR_AAD$PerSur <- SUR_AAD$PerSur/100

# summary, mean by site and species


avgSUR_tire <-SUR %>%
  filter(PerSur >0, Diapause == "Y") %>%
  group_by(Number, Species) %>%
  summarise( mean_sur = mean(PerSur))
# write averge summary table file
##write.csv(avgSUR_tire, "Data/OW_avgSur_tire_14012021.csv")

#'### Plots
#'

# plot survial by location 
ggplot(filter(SUR, PerSur >0, Diapause == "Y"), aes(Location, PerSur, col = Species))+
  geom_boxplot() +
  ylab("Percent Survival (%)") +
  theme_linedraw()

#'### MODELS
#'
 
mixed1 <- lmer(PerSur ~ JANmeanT + (1|Number), family= beta, data= SUR_AAD)


