library(ggplot2)
library(ggmap)
library(tidyr)
library(dplyr)
setwd("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Maps_&_Viz/")
load("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/processed_data.RData")

latlongmap <- map_data("county") #load map w/ coords from ggmap
load("FIPS.RData") #load df with FIPS and state, county abbreviations
county_map<-left_join(latlongmap,st_co, by=c('subregion','region')) #merge them 
save(county_map, file="county_FIPS.RData") #save
ggplot()+geom_polygon(data=county_map, aes(x=long, y=lat, fill=region,group=group))+coord_fixed(1.3)+guides(fill=FALSE)


