library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(rlang)
setwd('~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Maps_&_Viz/coxcombv3/') #set WD
load('~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/processed_data.RData')
source('coxcFunction.R') #load function coxc

## CREATE NEW TRIBE DATA FOR ME TO WORK WITH FOR THIS COXCOMB
#1. Change "Time 1" to time1, and make it a factor so I can plot
coxcomb_tribes <- merged_data_tribe_long
coxcomb_tribes$time[coxcomb_tribes$time == "Time 1"] <- "time 1"
coxcomb_tribes$time[coxcomb_tribes$time == "Time 2"] <- "time 2"

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
coxc("n_unique_FIPS",sortdir="a",sortby=1,plottype=1) #call function


###JUSTIN'S NOTES
  - does not have first option I requested
  - can i create a new sortby, using diff as a percent. that way i can better compare tribes... i can just create new variables on my own.

#VARIABLES
# > ls(merged_data_record_all_long)
# [1] "amen_rank"     "avg_lat"       "avg_lon"       "built_env"     "crsi"          "FIPS"          "gas_avg"       "governance"    "ID"            "lat"           "lon"          
# [12] "mid_lat"       "mid_lon"       "n_unique_FIPS" "natural_env"   "oil_avg"       "precip"        "risk"          "society"       "time"          "tribe"      
