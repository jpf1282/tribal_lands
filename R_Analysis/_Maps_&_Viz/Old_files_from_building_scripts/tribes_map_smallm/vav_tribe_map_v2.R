library(ggplot2)
library(ggmap)
library(tidyr)
library(dplyr)
library(stringr)
library(rlang)
library(cowplot)

setwd("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Maps_&_Viz/tribes_map_smallm")
load('mdral.RData')
load("county_FIPS.RData")
county_map<-county_map %>% rename(cm_long=long, cm_lat = lat)
source("maps_small_multiples.R")

head(merged_data_record_all_long)
head(county_map)



##########selectTribes Function###########
#vor - variable of interest

#sortdir - direction to sort
# a: sort ascending
# d: sort descending

# sortby - what to sort
# 1: diff
# 2: time 1
# 3: tribe

#ntribes = number of tribes to display

tribe_list<-selectTribes("n_unique_FIPS",sortdir="a",sortby=3,ntribes1=188,ntribes2=188)
tribe_list

  ##########Call map function
# t[1 or 2]_c_type: 
  #1: (lat#, lon#)
  #2: (avg_lat_#, avg_lon_# )
  #3: (mid_lat_#, mid_lat_#) 

#z (zoom)
  #1 Zoom in option 1
  #2 Zooom in option 2
  #3 Western half of U.S.
  #4 Eastern half of U.S.
  #5 All U.S.

source("maps_small_multiples.R")
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Cocopah")) #one tribe
createmap(t1_c_type=2,t2_c_type=1,z=1,c("Umpqua", "Modoc","Comanche")) #three tribes by name
createmap(t1_c_type=2,t2_c_type=2,z=2,tribe_list) #tribes from selectTribe function
    

