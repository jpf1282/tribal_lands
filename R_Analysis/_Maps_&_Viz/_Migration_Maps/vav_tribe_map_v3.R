library(ggplot2)
library(ggmap)
library(tidyr)
library(dplyr)
library(stringr)
library(rlang)
library(cowplot)

# setwd() #don't need this right now
load('mdral.RData')
load("county_FIPS_v2.RData")

county_map<-county_map %>% rename(cm_long=long, cm_lat = lat)
source("maps_small_multiples_v3.R")

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
# 3: time 2    # I ADDED THIS
# 4: tribe

#ntribes = number of tribes to display

tribe_list<-selectTribes("n_unique_FIPS",sortdir="d",sortby=3,ntribes1=1,ntribes2=5)
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
createmap(t1_c_type=1,t2_c_type=1,z=1,tribe_list) #tribes from selectTribe function 

createmap(t1_c_type=1,t2_c_type=1,z=1,c("Nambe Pueblo")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Cocopah")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Hualapai")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Cahto")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Chitimacha", "Comanche")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Achomawi")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Comanche")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Modoc")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Umpqua")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=1,c("Paiute")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=2,c("Umpqua", "Modoc","Comanche", "Chitimacha")) #three tribes by name
createmap(t1_c_type=2,t2_c_type=1,z=2,c("A-wall-a-che","Agawams" )) #tribes from selectTribe function
    

