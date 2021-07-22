#! /usr/bin/env bash
# 2020-OCT-16
# E.A. Burakowski, Katie Susong
# Retrieves Nov-Apr (2018-2019) NOHRSC SNODAS .tar from National Snow and Ice Data Center (NSDIC)
# server (sidads.colorado.edu)
# 
# TO EDIT
#   months in line 16 format is "##_MMM" "##_MMM"
#   masked or unmasked change in line 18
#   year in line 18

# OTHER OPTIONS
#   use FTP to manually add files, depending on the number of files needed and the data range can be just as easy

cd OW_SNODAS
mkdir Data
cd Data

for months in "11_Nov" "12_Dec" 
    do
	  wget -N ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/2018/"$months"/*.tar
    done
    
for months in "01_Jan" "02_Feb" "03_Mar" "04_Apr"
    do
	  wget -N ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/2019/"$months"/*.tar
    done