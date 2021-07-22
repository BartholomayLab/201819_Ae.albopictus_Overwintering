README

# Data file associated with OW albopictus project 


##### Primary author: Katie Milligan Susong (KMS)
- Pathobiological Sciences-University of Wisconsin Madison
- Midwest Center of Excellence in Vector-Borne Disease
- ksusong@wisc.edu
##### PI: Lyric Bartholomay
- Pathobiological Sciences-University of Wisconsin Madison
- Midwest Center of Excellence in Vector-Borne Disease
- lyric.bartholomay@wisc.edu
--------------------------------------------------------------------------------------

##### Conventions:
- OW - Over Wintering, all OW albopictus data files 
- OWALL- over winter a albopictus monitor
- OWALLA- backup monitor data
- AVG- average of OWALL and OWALLA monitors 
- KMS - files created by Katie Milligan Susong
_###### - MMDDYY date created

------------------------------------------------------------------------------------------
#### Original data files: Passed on by Brad Tucker and Bieneke Bron

- HOBOall.csv          : full HOBO monitor data, TIRES ONLY, includes temperature and relative humidity
- OWhatch.xlsx         : hatch data, multiple sheets, not for use in R
- OWhatchALL.csv       : hatch data, formatted for use and import into R
- sitedetails.xlsx     : full information and notes on the study sites
- OWALL_030530.csv     : 1st Temperature Monitoring data for all field sites 
- OWALL_A030530.csv    : 2nd Temperature Monitoring data for all field sites 
- OWALL_AVG030530.csv  : AVG of 1st and 2nd Temperature Monitoring data for all field sites




#### Data file with formatting changed by KMS: changes include new DateTime format, time 
#### stamp rounding and repeat reordering (see 01 data formatting.R and 01 Functions.R)

- OWALL.f_KMS_092220.csv
- OWALLA.f_KMS_092220.csv
- OWALLAVG.f_KMS_092220.csv


#### Wide data table pivoted by DateTime, done by KMS (see 01 data formatting.R and 01 Functions.R)

- OWALL.wide_KMS_092220.csv
- OWALLA.wide_KMS_092220.csv
- OWALLAVG.wide_KMS_092220.csv



#### Study site and survival files with R friendly formatting, done by KMS 

- OWhatchALL.csv           : hatch data, formatted for use and import into R
- site_location.csv        : site info limited to location and ID number 
- site_points/site_points1 : a SpatialPoints object of the study sites (KMS 06_site_shp.R)



#### SNODAS Snow depth data https://nsidc.org/data/G02158/versions/1

- OWsitesnow.csv            : daily snow depth at the study sites (SNODAS_scripts/04_depthsitesSNODAS.R)
- SnowSite_07012021_kms.csv : daily snow depth with site specific details including 3hr-temperature



#### Summary statistic of OW albo. Survival, temperature and snow 

- OW_summary.cvs: includes all site summary statistics including:(KMS Scripts/04_Temperature_Summary_Data.R)
	- Number: site location ID
	- Strip: strip ID
	- Species: AT or AA
	- Egg.Count: total eggs on egg sheet
	- Total.Larvae: # hatched/ strip
	- PerSur: Percent Survival 
	-JANmeanT : Mean JAN temp in the tire, ºC
	- JANmeanA : Mean JAN temp ambient, ºC
	- JANmeanD : Mean JAN tire - ambient, ºC
	- DJFmeanT : Mean DIF temp in tire ºC
	- DJFmeanA : Mean DJF temp ambient, ºC
	- JANmeanD : Mean DJF tire - ambient, ºC
	- MinT : min temp in tire, ºC
	- MinA : min temp in ambient, ºC
	- MinD : Min T, tire - ambient, ºC
	- FFrostT : Date of first frost in tire
	- FFrostA : Date of first frost in ambient
	- DaysB12T : Days below -12ºC in tire
	- DaysB12A : Days below -12ºC ambient
	- DaysB0conT : continuous days below 0ºC in tire
	- DaysB0conA : continuous days below 0ºC ambient
	- HrsB12conT : continuous hrs below -12ºC in tire
	- HrsB21conA : continuous hrs below -12ºC in ambient
	- FreThaT : Number of freeze-thaw events in tire
	- FreThaA : Number of freeze-thaw events in ambient
	- Location : location of site 

- OW_avgSur_tire.csv : avg. survival by site (08_survival_analysis.r)
- Ow_avgSur_location.csv : avg, survival by location 


-----------------------------------------------------------------------------------------------------------

