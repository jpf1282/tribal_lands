
library(tidyverse)
library(dplyr)
library(janitor)

setwd("C:/Users/12088/Documents/GitHub/tribal_lands/R_Analysis")

# Import data from Jude's processing
load("C:/Users/12088/Documents/GitHub/tribal_lands/R_Analysis/final_data.Rdata")
load("C:/Users/12088/Documents/GitHub/tribal_lands/R_Analysis/processed_data.RData")


# tribe_cce_variables -----------------------------------------------------


# Add in soil data
soil <- read_csv("C:/Users/12088/Documents/GitHub/tribal_lands/R_Analysis/county_soil_stats.csv") %>%
  clean_names() %>%
  select(FIPS=fips,soc=sand_mean)

# Add in topography data
topo <- read_csv("C:/Users/12088/Documents/GitHub/tribal_lands/R_Analysis/topo_us.csv") %>%
  select(FIPS=geoid,elevation=elevation_mean,tri=tri_mean)

#Merge topo and soil data
analysis_ds <- merged_data_record_all_long %>%
  inner_join(topo) %>%
  inner_join(soil)

# FINAL DATAFRAME
data_long <- distinct(analysis_ds, tribe, time, FIPS, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(tribe = factor(tribe),
         time = factor(time),
         oil_avg = oil_avg/1000,   #converting to thousands
         gas_avg = gas_avg/1000000) %>%   #converting raw data in thousands to billions
  dplyr::select(tribe,
                FIPS,
                n_unique_FIPS,
                time,
                lon,
                lat,
                oil_avg,
                og_basin,
                gas_avg,
                og_well_count,
                precip,
                h_100_hist,
                drt_median,
                p_all,
                fire_mean,
                elevation,
                tri,
                soc)

#write_csv()


# tribe_cce_distances -----------------------------------------------------

distance_avg <- select(merged_data_tribe, avg_dist)

distance_min_max <- select(merged_data_record, dist) %>%
  group_by(tribe) %>%
  summarize(min_dist = min(dist),
            max_dist = max(dist)) %>%
  as_data_frame()

# FINAL DATAFRAME
distance_final <- left_join(distance_min_max, distance_avg)
                              


