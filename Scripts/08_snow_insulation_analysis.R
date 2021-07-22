#' ---
#' title: " 07.b OW albopictus- Snow Insulation Data, "
#' author: "Katie Susong"
#' date: "07 January 2021"
#' ---

# set wd
setwd("~/Documents/CBS_PhD/albopictus_OW/")

#libraries
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(dplyr))
shhh(library(lubridate))
shhh(library(tidyr))
shhh(library(ggplot2))
shhh(library(GGally))
shhh(library(forestmodel))
shhh(library(lme4))
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(sjstats)
library(effects)

# import data 
DIA <- read.csv("Data/SnowSite_07012021_kms.csv")
DIA$date <- ymd(DIA$date) # correct data format
#fix location order to STD 
DIA$Location <- factor(DIA$Location , levels=c("New Berlin", "NWMAD","SCCMAD" ,"Kankakee", "Champaign-Urbana"))

w.DIA <- pivot_longer(DIA,cols = c(MeanT_Ambient, MeanT_Tire, snow_depth))

#' Ayg. depths by Location 
#'===========================
#'
filter(DIA, date >"2018-12-31" , date < "2019-02-01" ) %>%
  group_by(Location) %>%
  summarise(JANmean_mm = mean(snow_depth), JANmed_mm = median(snow_depth), 
            JANmax_mm = max(snow_depth), JANmin_mm = min(snow_depth))

#' Inversigation Plots
#' ===================
#' 

# corrolations 
ggpairs(DIA)

#response variable distribution
hist(DIA$MeanT_Tire)

# snow time serise
DIA%>%
  filter(date >= "2018-12-01", date <= "2019-03-31") %>%
  ggplot( aes(date,snow_depth))+
  geom_line() +
  facet_wrap(~Location, ncol = 1) +
  ylab("Snow Depth (mm)") +
  xlab("Date (ymd)") +
  theme_linedraw() +
  annotate("rect", xmin = as.Date("2019/01/01"), xmax = as.Date("2019/01/31"),
           ymin =-Inf, ymax = Inf , 
           alpha = .5)

  
# Mean Tire to Mean Ambient + snow depth + location
ggplot(DIA, aes(MeanT_Tire, MeanT_Ambient, col = snow_FAC10))+
  geom_point()+
  xlab("Mean daily temperature, Tire (C??)") +
  ylab("Mean daily temperature, Ambient (C??)") +
  facet_wrap(~Location, ncol = 5) +
  theme_bw()

# Mean Tire to Mean Ambient + snow depth
ggplot(DIA, aes(MeanT_Tire, MeanT_Ambient, col = Location))+
  geom_point()+
  xlab("Mean daily temperature, Tire (C??)") +
  ylab("Mean daily temperature, Ambient (C??)") +
  theme_bw() +
  ylim(-30,0) +
  xlim(-30,0) +
  geom_vline(xintercept = -12) +
  geom_hline(yintercept = -12)

# Mean Tire to Mean Ambient + snow depth + location
ggplot(DIA, aes(MeanT_Tire, MeanT_Ambient, col= Location))+
  geom_point()+
  facet_grid(~snow_FAC10) +
  xlab("Mean daily temperature, Tire (C)") +
  ylab("Mean daily temperature, Ambient (C)") +
  theme_bw()

# Mean difference to Mean Ambient + snow depth + location
ggplot(DIA, aes(MeanD, MeanT_Ambient, col= Location))+
  geom_point()+
  facet_grid(~snow_FAC10) +
  xlab("Mean Difference (C??)") +
  ylab("Mean daily temperature, Ambient (C??)") +
  theme_bw()

# Jan snow depth by location 
DIA%>%
  filter(date >= "2019-01-01", date<= "2019-01-31") %>%
  ggplot( aes(Location, snow_depth))+
  geom_boxplot()

#' Model/Significance
#' =================
#' 
#'### Catigorical snow cover
#'
#' These models use the catagorical sections of 100mm snow depths. 
#' 
#' **simple.mean**   MeanT_Tire ~ MeanT_Ambient
#' **one.mean**      MeanT_Tire ~ MeanT_Ambient + snow_FAC10
#' **two.mean**      MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + Location
#' **mixed.model**   MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + 1|Location
#' **mixed.model2**  MeanT_Tire ~ MeanT_Ambient + snow_FAC10 + 1|site
#' 
#' 

acf(DIA)


simple.mean <- glm(MeanT_Tire ~ MeanT_Ambient, data = DIA )
summary(simple.mean)

one.mean <- glm(MeanT_Tire ~ MeanT_Ambient + relevel(snow_FAC10, ref = 5), data = DIA )
summary(one.mean)
plot_model(one.mean)
forest_model(one.mean)

two.mean <- glm(MeanT_Tire ~ MeanT_Ambient + relevel(snow_FAC10, ref = 5)+Location, data = DIA )
summary(two.mean)
forest_model(two.mean)

mixed.model <- lmer(MeanT_Tire ~ (1|Location) + MeanT_Ambient + relevel(snow_FAC10, ref = 5), data = DIA)
summary(mixed.model)
tab_model(mixed.model)

mixed.model2 <- lmer(MeanT_Tire ~ MeanT_Ambient + relevel(snow_FAC10, ref = 5)+ (1|site), data = DIA)
summary(mixed.model2)
tab_model(mixed.model2)
plot(mixed.model2)

mixed.model3 <- lmer(MeanT_Tire ~ MeanT_Ambient + relevel(snow_FAC10, ref = 5)+ (1|site) + (1|Location), data = DIA)
tab_model(mixed.model3)

mixed.model4 <- lmer(MeanT_Tire ~ MeanT_Ambient + relevel(snow_FAC10, ref = 5)+ (1|Location), data = DIA)
tab_model(mixed.model3)



#comparison of models 
anova(one.mean,simple.mean)
anova(mixed.model,one.mean)
anova(two.mean, one.mean)
anova(mixed.model3,mixed.model2)
anova(mixed.model2,mixed.model4)

#'### Continuous models 
#'
#'Snow is continued as a continuous value
#'
#'
mean.1 <-  glm(MeanT_Tire ~ MeanT_Ambient + snow_depth , data = DIA )
summary(mean.1)
forest_model(mean.1)


mean.2 <-  glm(MeanT_Tire ~ MeanT_Ambient + snow_depth + Location , data = DIA )
summary(mean.2)
forest_model(mean.2)

anova(mean.1, mean.2)
