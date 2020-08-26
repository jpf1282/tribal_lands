library(pacman)
p_load(tidyverse,sf,USAboundaries)

#load data
load("R_Analysis/final_data.Rdata")


#Constructing datasets
data_long <- distinct(merged_data_record_all_long, tribe, time, FIPS, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(tribe = factor(tribe),
         time = factor(time)) 

#Subset of counties in T1 - Shannon Co SD changed to 46102 Oglala Lakota
counties.t1 <- unique(filter(data_long, time=="time 1")$FIPS) %>%
  str_replace_all("46113","46102")

#Grab county shapes from USAboundaries
co <- us_counties(resolution = "low") %>%
  filter(!(state_abbr %in% c("AK","HI","PR"))) %>%
  st_transform(2163)

#Subset based on T1 counties
co_select <- co %>%
  filter(geoid %in% counties.t1)

#Check if differences exist
setdiff(counties.t1,co_select$geoid)

#Calculate ratio of selected counties to all CONUS
co_area <- sum(st_area(co_select))/sum(st_area(co))

#Convert from meters squared to square miles
measurements::conv_unit(sum(st_area(co_select)),"m2","mi2")
measurements::conv_unit(sum(st_area(co)),"m2","mi2")
