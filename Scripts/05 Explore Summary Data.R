#' ---
#' title: "05 OW albopictus Explore Summary Data"
#' author: "Katie Susong"
#' date: "12th October 2020"
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
summary.w  <- pivot_wider(summary.sh, names_from = c(Species, Diapause), values_from =  PerSur )
print(summary.w)


# summary for AA, diapause without controls
AA2 <- filter(summary,Species == "AA", Diapause == "Y", Location != c("Control27","Control4"))
# fix factor levels
AA2$Location <- factor(AA2$Location , levels=c("New Berlin", "NWMAD","SCCMAD" ,"Kankakee", "Champaign-Urbana"))

# add std % survival 
## observed survival / optimal survival (@ 4C, = 12.01) * 100
AA2$std.sur <- AA2$PerSur / 12.01 * 100
#'### SUMMARY VALUES
#'

#'#### Summary of Jan mean for AA survival 
#'
#'Summary is for diapausing eggs for AA and then for AT
summary %>%
  filter(Total.Larvae >0, Species == "AA", Diapause == "Y") %>%
  summarise(meanT=mean(JANmeanT), meanA= mean(JANmeanA))

summary %>%
  filter(Total.Larvae >0, Species == "AT", Diapause == "Y") %>%
  summarise(meanT=mean(JANmeanT), meanA= mean(JANmeanA))

#'#### Summary of AA with survival 
#'
#' Only includes diapausing AA egg strips

summary %>%
  filter( PerSur >0, Species == "AA", Diapause == "Y") %>%
  summarise(Number = Number, Strip = Strip, Location = Location, PerSur = PerSur)
#' The survival of site compared to the optimal (4C) condition 
#' 

x <- c(0.09, 0.01, 2.01, 1.60, 0.055)
round((x/12.01)*100,2) # (strip survival / avg. optimal survival) *100


#'#### Summary of AT Survial 
#'
#'##### All AT strips at New Berlin
#'
#' Includes summary of all strips at Bew Berlin with AT, the mean and varience of surival fro diaupause eggs 
#' and the mean and varience for non-diapause eggs
#' 
#total summary
summary %>%
  filter(Species == "AT", Location == "New Berlin") %>%
  summarise(Number = Number, Strip = Strip, Location = Location,Diapause = Diapause, PerSur = PerSur)

#mean and varience
summary %>%
  filter(Species == "AT", Location == "New Berlin") %>%
  group_by(Diapause) %>%
  summarise(MeanSur = mean(PerSur), sdSur = sd(PerSur))

#'##### All AT strips at NWMAD
#'
#' Includes summary of all strips at NWMAD with AT, the mean and varience of surival fro diaupause eggs 
#' and the mean and varience for non-diapause eggs
#' 
#total summary
summary %>%
  filter(Species == "AT", Location == "NWMAD") %>%
  summarise(Number = Number, Strip = Strip, Location = Location,Diapause = Diapause, PerSur = PerSur)

#mean and varience
summary %>%
  filter(Species == "AT", Location == "NWMAD") %>%
  group_by(Diapause) %>%
  summarise(MeanSur = mean(PerSur), sdSur = sd(PerSur))

#'##### All survival summary diapause and non-dipause 
#'

mean.sur <-summary%>%
  group_by(Diapause, Species, Location )%>%
  summarise(MeanSur = mean(PerSur, na.rm = T), sdSur = sd(PerSur, na.rm = T))

#'##### TIre summary stats for artical 
#'
#' Includes the key summary stats that will be included in the artical and used to generate the final report

# art.sum <- summary %>%
#  group_by(Number) %>%
#  summarise( JANmeanT = mean(JANmeanT),
#             JANmeanA = mean(JANmeanA),
 #            MinT = mean(MinT),
  #           MinA = mean(MinA),
   #          DaysB12T = mean(DaysB12T),
    #         DaysB12A = mean(DaysB12A),
     #        hrB12Tcon = mean(HrsB12conT),
      #       hrB12Acon = mean(HrsB12conA))

# write.csv(art.sum, "bytire_summary_temp_20210126KMS.csv")

#'## PLOTS
#'
#'### Basic Box plot 
#'
ggplot( filter(summary, PerSur > 0), aes(Location, PerSur, col = Species))+
  geom_boxplot()

#'### Survival Verse Mean Jan Temperature 
#'

a <- ggplot(AA2, aes(JANmeanT, PerSur,col = Location))+
  geom_point(size = 3) +
  geom_vline(xintercept = -2.5, linetype = "dashed") +
  theme_classic() +
  xlab("Mean Tire Temperature for Jan. (??C)") +
  ylab("Percent Survival") +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ylim(0,4) +
  xlim(-10,0) +
  geom_label(label=" NWMAD, sheet 6 & 28", 
             x=-1.2,
             y=2.4,
             label.padding = unit(0.25, "lines"), # Rectangle size around label
             label.size = .2,
             color = "black",fill="#56B4E9", size = 2.5)


b <- ggplot(AA2, aes(JANmeanA, PerSur,col = Location))+
  geom_point(size = 3) +
  geom_vline(xintercept = -2.5, linetype = "dashed") +
  theme_classic() +
  xlab("Mean Ambient Temperature for Jan. (??C)") +
  ylab("Percent Survival") +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ylim(0,4) +
  xlim(-10,0) +
  geom_label(label=" NWMAD, sheet 6 & 28", 
             x=-5,
             y=2.4,
             label.padding = unit(0.25, "lines"), # Rectangle size around label
             label.size = .2,
             color = "black",fill="#56B4E9", size = 2.5)

grid.arrange(a,b)

#'### STD Survival Verse Mean Jan Temperature 
#'

a <- ggplot(AA2, aes(JANmeanT, std.sur,col = Location))+
  geom_point(size = 3) +
  geom_vline(xintercept = -2.5, linetype = "dashed") +
  theme_linedraw() +
  xlab("Mean Tire Temperature for Jan. (??C)") +
  ylab("STD Percent Survival") +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ylim(0,15) +
  xlim(-10,0) 
  


b <- ggplot(AA2, aes(JANmeanA, std.sur,col = Location))+
  geom_point(size = 3) +
  geom_vline(xintercept = -2.5, linetype = "dashed") +
  theme_linedraw() +
  xlab("Mean Ambient Temperature for Jan. (??C)") +
  ylab("STD Percent Survival") +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ylim(0,15) +
  xlim(-10,0) 
  
grid.arrange(a,b)

# survival vs days below -12
c <- ggplot(AA2, aes(DaysB12T, PerSur,col = Location))+
  geom_point(size = 3) +
  theme_classic() +
  xlab("Total Days Below -12??C, Tires") +
  ylab("Percent Survival") +
  ylim(0,5) +
  xlim(0,30) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  geom_label(label=" NWMAD, sheet 6 & 28", 
             x= 1.3,
             y=2.4,
             label.padding = unit(0.25, "lines"), # Rectangle size around label
             label.size = .2,
             color = "black",fill="#56B4E9", size = 2.5)

d <- ggplot(AA2, aes(DaysB12A, PerSur,col = Location))+
  geom_point(size = 3) +
  theme_classic() +
  xlab("Total Days Below -12??C, Ambient") +
  ylab("Percent Survival")+
  ylim(0,5) +
  xlim(0,30) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  geom_label(label=" NWMAD, sheet 6 & 28", 
             x= 14,
             y=2.4,
             label.padding = unit(0.25, "lines"), # Rectangle size around label
             label.size = .2,
             color = "black",fill="#56B4E9", size = 2.5)

grid.arrange(c,d)

#survival vs contig. hours below -12
e <- ggplot(AA2, aes(DaysB12conT, PerSur,col = Location))+
  geom_point(size = 3) +
  geom_vline(xintercept = 16, linetype = "dashed")+
  xlab("Continuous Hours Below -12??C,Tires") +
  ylab("Percent Survival") +
  ylim(0,4) +
  xlim(0,80) +
  theme_classic() +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  geom_label(label=" NWMAD, sheet 6 & 28", 
             x=5,
             y=2.3,
             label.padding = unit(0.25, "lines"), # Rectangle size around label
             label.size = .2,
             color = "black",fill="#56B4E9", size = 2.5)


f <- ggplot(AA2, aes(DaysB12conA, PerSur,col = Location))+
  geom_point(size = 3) +
  geom_vline(xintercept = 16, linetype = "dashed") +
  theme_classic() +
  xlab("Continuous Hours Below -12??C, Ambient") +
  ylab("Percent Survival")+
  ylim(0,3) +
  xlim(0,80) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  geom_label(label=" NWMAD, sheet 6 & 28", 
    x=73,
    y=2.2,
    label.padding = unit(0.25, "lines"), # Rectangle size around label
    label.size = .2,
    color = "black",fill="#56B4E9", size = 2.5)

grid.arrange(e,f)
