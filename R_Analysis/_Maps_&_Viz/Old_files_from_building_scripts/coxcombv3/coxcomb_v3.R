library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(rlang)
setwd('/Users/hemanthnair/Dropbox (Vis-à-Vis IVD)/VisaVis/Clients/Upwork/JFarrell/vav') #set WD
load('mdral.RData')
source('coxcFunction.R') #load function coxc

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
coxc("n_unique_FIPS",sortdir="a",sortby=3,plottype=2) #call function



#VARIABLES
# > ls(merged_data_record_all_long)
# [1] "amen_rank"     "avg_lat"       "avg_lon"       "built_env"     "crsi"          "FIPS"          "gas_avg"       "governance"    "ID"            "lat"           "lon"          
# [12] "mid_lat"       "mid_lon"       "n_unique_FIPS" "natural_env"   "oil_avg"       "precip"        "risk"          "society"       "time"          "tribe"      
