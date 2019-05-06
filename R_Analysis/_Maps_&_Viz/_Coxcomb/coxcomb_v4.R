library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(rlang)
setwd('~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Maps_&_Viz/_Coxcomb/') #set WD
load('~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/processed_data.RData')
coxcomb_tribes <- merged_data_record_all_long
source('coxcFunction_v4.R') #load function coxc

## [UNDER CONSTRUCTION]....CREATE NEW TRIBE DATA FOR ME TO WORK WITH FOR THIS COXCOMB
# # coxcomb_tribes$time[coxcomb_tribes$time == "Time 1"] <- "time 1"
# coxcomb_tribes$time[coxcomb_tribes$time == "Time 2"] <- "time 2"
# This above is still under construction. I'll likely use this type of code for final visualizations. 

# coxc creates coxcomb visualizations of variables for time and time2 either overlayed or side by side
# the function first sorts "merged_data_record_all_long" by diff (time2-time1), time 1, or tribe, ascending or descending.
# It then displays the first 25 of the sorted data frame, overlayed or side by side

########################################## PARAMETERS ####################################
#coxc(vor,sortdir="d",sortby=2,plottype=2) -->function call
#vor - variable of interest
#sortdir - direction to sort
  # a: sort ascending
  # d: sort descending
# sortby - what to sort
  # 1: diff
  # 2: time 1
  # 3: tribe
# plottype - type of coxcomb
  # 1: overlayed
  # 2: side by side
########################################## CALL FUNCTION ####################################
coxc("shel_injur",sortdir="a",sortby=1,plottype=2,ntribes1=1,ntribes2=15) #call function





###JUSTIN'S NOTES
  - does not have first option I requested
  - can i create a new sortby, using diff as a **percent change** that way i can better compare tribes... i can just create new variables on my own.
  - fiddle with the opacity so its not as overlapping...

#VARIABLES
> ls(merged_data_record_all_long)
[1] "amen_rank"     "avg_lat"       "avg_lon"       "built_env"     "crsi"          "FIPS"          "gas_avg"       "governance"    "ID"           
[10] "lat"           "lon"           "mid_lat"       "mid_lon"       "n_unique_FIPS" "natural_env"   "oil_avg"       "p_all"         "p_blm"        
[19] "p_dod"         "p_doe"         "p_nps"         "p_usfs"        "p_usfws"       "precip"        "risk"          "shel_crop"     "shel_crop_pc" 
[28] "shel_dura"     "shel_fatal"    "shel_fatal_pc" "shel_injur"    "shel_injur_pc" "shel_prop"     "shel_prop_pc"  "shel_yr_total" "society"      
[37] "time"          "tribe"  