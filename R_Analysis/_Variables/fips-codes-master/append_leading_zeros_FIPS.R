library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(scales)
library(rgdal)
library(geosphere)
library(leaflet)
library(leaflet.extras)

setwd("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Variables/fips-codes-master/")

county_fips_master <- read_csv("county_fips_master.csv")

county_fips_master_with_zeros <- mutate(county_fips_master,
                        fips = str_pad(fips, 5, pad = "0", side = "left"))

write_csv(county_fips_master_with_zeros, "county_fips_master_with_zeros.csv")
