
# --- Project Prep ----------------------------------------

# Load library
# Install these packages if neccessary
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(scales)
library(rgdal)
library(sp)
library(geosphere)
library(leaflet)
library(leaflet.extras)

# Set working directory - change to your project directory
# setwd("~/")


# --- Import Data ---------------------------------------

# Import data using readr package
# Use explicit col_types - update as needed
tribes_0 <- read_csv("tribes.csv", col_types = "ccccc")
amenity_0 <- read_csv("natural_amenity.csv", col_types = "ci")

# --- Check Data ------------------------------------------

# Check data for unmatched FIPS
# Create list of unique FIPS and no. of digits
tribes_fips <- distinct(tribes_0, FIPS) %>%
  mutate(nchar = nchar(FIPS))
amenity_fips <- distinct(amenity_0, FIPS) %>%
  mutate(nchar = nchar(FIPS))

# Join to find out what is unmatched
# in tribes but not amenity
anti_join(tribes_fips, amenity_fips) %>% 
  View()

# in amenity but not tribes
anti_join(amenity_fips, tribes_fips) %>%
  View()

# Subset tribes data to time 1 and time 2
tribes_pre <- filter(tribes_0, time == "time1")
tribes_post <- filter(tribes_0, time == "time2")

# Check for duplicate records
# Currenlty dupcliates are include in the rest of the analyses
group_by(tribes_pre, tribe, time, FIPS, fips.name, state_from_fips) %>% 
  count() %>%
  arrange(-n) %>%
  filter(n > 1) %>%
  View()

group_by(tribes_post, tribe, time, FIPS, fips.name, state_from_fips) %>% 
  count() %>%
  arrange(-n) %>%
  filter(n > 1) %>% 
  View()

# --- Wrangle Data ----------------------------------------

# Keep distinct FIPS
tribes_dedup <- distinct(tribes_0)
amenity_dedup <- distinct(amenity_0)

# Download census county shapefile from US census website
# Put this into your project folder
# http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_20m.zip

# Import shape files
# Use this if shape files it its own subfolder cb_2017_us_county_20m
# shape <- readOGR('cb_2017_us_county_20m', "cb_2017_us_county_20m")
shape <- readOGR('x_County_Shapefiles/cb_2017_us_county_20m.shp')

# Quick plot to check shapefile
plot(shape)

# Calculate centriods using centroid fn
shape_cent <- centroid(shape)

# Convert to data.frame
shape_cent_df <- as.data.frame(shape_cent) %>%
  rename(lat = V2,
         lon = V1)

# Add centriods lat and lon to original shapefile data.frame
shape@data$lat <- shape_cent_df$lat
shape@data$lon <- shape_cent_df$lon

# Map county and centroids using leaflet to check centroids
leaflet() %>%
  addTiles() %>%
  addPolygons(data = shape, weight = .2) %>% # Map county boundaries
  addCircles(data = shape@data, lng = ~lon, lat = ~lat, color = "orange") # Map centriods

# Export data.frame from shape file for processing
shape_df <- shape@data

# Create FIPS from state and county code
shape_df <- mutate(shape_df, FIPS = paste0(STATEFP, COUNTYFP))

# Merge with amenity score file

# Check
anti_join(shape_df, amenity_dedup, by = "FIPS") %>% View()
anti_join(amenity_dedup, shape_df, by = "FIPS") %>% View()

# Append leading 0 back to FIPS
amenity_dedup <- mutate(amenity_dedup,
  FIPS_r = str_pad(FIPS, 5, pad = "0", side = "left"))

tribes_dedup <- mutate(tribes_dedup,
  FIPS_r = str_pad(FIPS, 5, pad = "0", side = "left"))

# Check
anti_join(shape_df, amenity_dedup, by = c("FIPS" = "FIPS_r")) %>% View()
anti_join(amenity_dedup, shape_df, by = c("FIPS_r" = "FIPS")) %>% View()

# Merge amenity scores with shapefile that has the centroid 
# using FIPS_r with the leading 0
amenity_dedup <- left_join(amenity_dedup, shape_df, by = c("FIPS_r" = "FIPS"))

# Remove the non leading 0 FIPS
# Make the leading 0 as the FIPS
amenity_dedup <- rename(amenity_dedup,
  FIPS_original = FIPS,
  FIPS = FIPS_r) %>%
  select(-FIPS_original)

tribes_dedup <- rename(tribes_dedup,
  FIPS_original = FIPS,
  FIPS = FIPS_r) %>%
  select(-FIPS_original)

# FIPS code change for
# see http://www.nws.noaa.gov/om/notification/scn15-31shannon_oglalacty%20cca.htm
# 46113 has been changed to 46102

# Subset tribes data to time 1 and time 2
tribes_pre <- filter(tribes_dedup, time == "time1")
tribes_post <- filter(tribes_dedup, time == "time2")

# Merge amenity rank scores with pre and post tribes data by FIPS
tribes_pre <- left_join(tribes_pre, amenity_dedup, by = "FIPS")
tribes_post <- left_join(tribes_post, amenity_dedup, by = "FIPS")

# Combine pre and post data side by side by tribe names
# Use full join to keep all tribes, including those with only time 1 or 2 data
merged_data <- full_join(tribes_pre, tribes_post, by = "tribe", suffix = c("_t1", "_t2"))

# Flag time 1 and 2, time 1 only, and time 2 only tribes
merged_data <- mutate(merged_data, 
  time = ifelse(!is.na(time_t1) & !is.na(time_t2), "Time 1 and 2", 
             ifelse(is.na(time_t1), "Time 2 only", "Time 1 only"))) %>%
  select(-time_t1, -time_t2) # Remove redundant time indicator

# Calculate average tribe amenity rank scores
merged_data <- group_by(merged_data, tribe) %>%
  mutate(avg_amen_rank_t1 = mean(amen_rank_t1, na.rm = TRUE),
         avg_amen_rank_t2 = mean(amen_rank_t2, na.rm = TRUE))

# --- Calculate Change Scores -----------------------------

# Calculate record (row) level change scores
merged_data <- mutate(merged_data,
  amenity_change_score = amen_rank_t1 - amen_rank_t2)

# Calculate tribe level change scores
merged_data <- mutate(merged_data,
  avg_amenity_change_score = avg_amen_rank_t1 - avg_amen_rank_t2)

# Rearrange column and row
merged_data <- select(merged_data, tribe, time, 
  starts_with("FIPS_"), starts_with("fips.name"), starts_with("state_from_fips"),
  amen_rank_t1, amen_rank_t2, amenity_change_score,
  avg_amen_rank_t1, avg_amen_rank_t2, avg_amenity_change_score,
  lat_t1, lon_t1, lat_t2, lon_t2
  ) %>%
  arrange(time)

# Fn to calculate geographic mid-point
# Fn adapted from https://livefreeordichotomize.com/2018/06/27/bringing-the-family-together-finding-the-center-of-geographic-points-in-r/
geographic_midpoint <- function(lon, lat, weight = NULL) {
  if (is.null(weight)) {
    weight <- rep(1, length(lon))
  }
  # degrees to radians
  lat <- lat * pi / 180
  lon <- lon * pi / 180
  # cartesian coordinates
  x <- cos(lat) * cos(lon)
  y <- cos(lat) * sin(lon)
  z <- sin(lat)
  # weighted mean
  x <- weighted.mean(x, w = weight)
  y <- weighted.mean(y, w = weight)
  z <- weighted.mean(z, w = weight)
  # convert to lat and lon
  lon <- atan2(y, x) * 180 / pi
  hyp <- sqrt(x * x + y * y)
  lat <- atan2(z, hyp) * 180 / pi
  
  data.frame(mid_lon = lon, mid_lat = lat)
}

# Apply the fn by tribe and time point to get mid points
# Time 1 mid_point
mid_pt_data_t1 <- group_by(merged_data, tribe) %>%
  do(geographic_midpoint(.$lon_t1, .$lat_t1)) %>%
  rename(mid_lon_t1 = mid_lon,
         mid_lat_t1 = mid_lat)

# Time 2 mid_point
mid_pt_data_t2 <- group_by(merged_data, tribe) %>%
  do(geographic_midpoint(.$lon_t2, .$lat_t2)) %>%
  rename(mid_lon_t2 = mid_lon,
         mid_lat_t2 = mid_lat)

# Merge mid_point data back with main data.frame
merged_data <- left_join(merged_data, mid_pt_data_t1, by = "tribe") %>%
  left_join(mid_pt_data_t2, by = "tribe")

# Make summary data at tribe level
# Count # of records and # of unique FIPS in each tribe
merged_data_tribe <- group_by(merged_data, tribe, time) %>%
  summarize(
    avg_amen_rank_t1 = mean(amen_rank_t1, na.rm = TRUE),
    avg_amen_rank_t2 = mean(amen_rank_t2, na.rm = TRUE),
    avg_lat_t1 = mean(lat_t1, na.rm = TRUE),
    avg_lon_t1 = mean(lon_t1, na.rm = TRUE),
    avg_lat_t2 = mean(lat_t2, na.rm = TRUE),
    avg_lon_t2 = mean(lon_t2, na.rm = TRUE),
    mid_lat_t1 = mean(mid_lat_t1, na.rm = TRUE),
    mid_lon_t1 = mean(mid_lon_t1, na.rm = TRUE),
    mid_lat_t2 = mean(mid_lat_t2, na.rm = TRUE),
    mid_lon_t2 = mean(mid_lon_t2, na.rm = TRUE),
    n_record = n(),
    n_unique_FIPS_t1 = n_distinct(FIPS_t1),
    n_unique_FIPS_t2 = n_distinct(FIPS_t2)) %>%
  arrange(time)

# Create tribe level summary meta info data.frame
merged_data_meta <- select(merged_data_tribe,
    -avg_amen_rank_t1, -avg_amen_rank_t2,
    -mid_lat_t1, -mid_lon_t1, 
    -mid_lat_t2, -mid_lon_t2)

# Join meta information from summary to record level data
merged_data_record <- left_join(merged_data, merged_data_meta, by = c("tribe", "time"), suffix = c("_r", "_tr")) 

# Lists tribes with both time 1 and 2, time 1 only or time 2 only data
tribes_time1and2_lst <- filter(merged_data_record, time == "Time 1 and 2") %>% distinct(tribe)
tribes_time1only_lst <- filter(merged_data_record, time == "Time 1 only") %>% distinct(tribe)
tribes_time2only_lst <- filter(merged_data_record, time == "Time 2 only") %>% distinct(tribe)

# --- Calculate Distance between Centroids ----------------

# Calculate distance in km using distHaversine fn
# This calcluate the shortest distance between two-points or the great-circle-distance

# Calculate the pairwise distance in km
merged_data_record <- merged_data_record %>% 
  mutate(dist = distHaversine(cbind(lon_t1, lat_t1), cbind(lon_t2, lat_t2)) / 1000)

# Calcuate the distance between tribe centriods in km
merged_data_record <- merged_data_record %>% 
  mutate(
    avg_dist = distHaversine(cbind(avg_lon_t1, avg_lat_t1), cbind(avg_lon_t2, avg_lat_t2)) / 1000,
    mid_dist = distHaversine(cbind(mid_lon_t1, mid_lat_t1), cbind(mid_lon_t2, mid_lat_t2)) / 1000)

# Calculate the between tribe centriods in km
merged_data_tribe <- merged_data_tribe %>% 
  mutate(
    avg_dist = distHaversine(cbind(avg_lon_t1, avg_lat_t1), cbind(avg_lon_t2, avg_lat_t2)) / 1000,
    mid_dist = distHaversine(cbind(mid_lon_t1, mid_lat_t1), cbind(mid_lon_t2, mid_lat_t2)) / 1000)


# --- Export Data -----------------------------------------

write_csv(merged_data_record, "amenity_records_level.csv")
write_csv(merged_data_tribe, "amenity_tribes_level.csv")


# --- Map Data --------------------------------------------

# Map county and centroids using leaflet
# Record Level
leaflet() %>%
  setView( # Set default view and zoom  
    lng = mean(merged_data_record$lon_t1, na.rm = TRUE),
    lat = mean(merged_data_record$lat_t1, na.rm = TRUE), 
    zoom = 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # use a black and white map layer
  addPolygons(data = shape, weight = .2) %>% # Map county boundaries
  addCircles(data = merged_data_record, lng = ~lon_t1, lat = ~lat_t1, 
    color = "black", fillOpacity = .3, group = "Time 1") %>% # Map Time 1 centriods
  addCircles(data = merged_data_record, lng = ~lon_t2, lat = ~lat_t2, 
    color = "orange", fillOpacity = .3, group = "Time 2") %>% # Map Time 2 centriods 
  addLayersControl( # Add checkbox to show or hide time 1 or 2 layer
    options = layersControlOptions(collapsed = FALSE),
    overlayGroups = c("Time 1", "Time 2"))

# Tribe Level (compare mean/avg point and mid point)
leaflet() %>%
  setView( # Set default view and zoom  
    lng = mean(merged_data_tribe$mid_lon_t1, na.rm = TRUE),
    lat = mean(merged_data_tribe$mid_lat_t1, na.rm = TRUE), 
    zoom = 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # use a black and white map layer
  addPolygons(data = shape, weight = .2) %>% # Map county boundaries
  addCircles(data = merged_data_tribe, lng = ~mid_lon_t1, lat = ~mid_lat_t1, 
    color = "black", fillOpacity = .5, group = "Time 1 mid point") %>% # Map Time 1 centriods midpoint
  addCircles(data = merged_data_tribe, lng = ~mid_lon_t2, lat = ~mid_lat_t2, 
    color = "orange", fillOpacity = .5, group = "Time 2 mid point") %>% # Map Time 2 centriods midpoint
  addCircles(data = merged_data_tribe, lng = ~avg_lon_t1, lat = ~avg_lat_t1, 
    color = "blue", fillOpacity = .5, group = "Time 1 avg point") %>% # Map Time 1 centriods avgpoint
  addCircles(data = merged_data_tribe, lng = ~avg_lon_t2, lat = ~avg_lat_t2, 
    color = "purple", fillOpacity = .5, group = "Time 2 avg point") %>% # Map Time 2 centriods avgpoint
  addLayersControl( # Add checkbox to show or hide time 1 or 2 layer
    options = layersControlOptions(collapsed = FALSE),
    overlayGroups = c("Time 1 mid point", "Time 2 mid point", "Time 1 avg point", "Time 2 avg point"))

# --- Reshape Data ----------------------------------------

# Reshape data from wide to long for mapping - Record all pairwise-level

# Create ID to map connected lines for each pair
merged_data_record_all <- mutate(merged_data_record, ID = row_number())

# Reshape data from wide to long for mapping
merged_data_record_all_t1 <- select(merged_data_record_all, ID, tribe, FIPS_t1, n_unique_FIPS_t1, amen_rank_t1, lon_t1, lat_t1, avg_lat_t1, avg_lon_t1, mid_lat_t1, mid_lon_t1) %>% 
  mutate(time = "time 1") %>%
  rename(FIPS = FIPS_t1,
         n_unique_FIPS = n_unique_FIPS_t1,
         amen_rank = amen_rank_t1,
         lon = lon_t1,
         lat = lat_t1,
         avg_lon = avg_lon_t1,
         avg_lat = avg_lat_t1,
         mid_lon = mid_lon_t1,
         mid_lat = mid_lat_t1)

merged_data_record_all_t2 <- select(merged_data_record_all, ID, tribe, FIPS_t2, n_unique_FIPS_t2, amen_rank_t2, lon_t2, lat_t2, avg_lat_t2, avg_lon_t2, mid_lat_t2, mid_lon_t2) %>% 
  mutate(time = "time 2") %>%
  rename(FIPS = FIPS_t2,
         n_unique_FIPS = n_unique_FIPS_t2,
         amen_rank = amen_rank_t2,
         lon = lon_t2,
         lat = lat_t2,
         avg_lon = avg_lon_t2,
         avg_lat = avg_lat_t2,
         mid_lon = mid_lon_t2,
         mid_lat = mid_lat_t2)

# Stack time 1 and time 2 into a long data.frame
# Use this data.frame for time 1 to time 2 all pairwise connecting lines map
merged_data_record_all_long <- rbind(merged_data_record_all_t1, merged_data_record_all_t2)


# Reshape data from wide to long for mapping - Average time 1 to record level
merged_data_record_t1 <- select(merged_data_record, tribe, FIPS_t1, n_unique_FIPS_t1, amen_rank_t1, lon_t1, lat_t1, avg_lat_t1, avg_lon_t1, mid_lat_t1, mid_lon_t1) %>% 
  mutate(time = "time 1") %>%
  rename(FIPS = FIPS_t1,
         n_unique_FIPS = n_unique_FIPS_t1,
         amen_rank = amen_rank_t1,
         lon = lon_t1,
         lat = lat_t1,
         avg_lon = avg_lon_t1,
         avg_lat = avg_lat_t1,
         mid_lon = mid_lon_t1,
         mid_lat = mid_lat_t1)

merged_data_record_t2 <- select(merged_data_record, tribe, FIPS_t2, n_unique_FIPS_t2, amen_rank_t2, lon_t2, lat_t2, avg_lat_t2, avg_lon_t2, mid_lat_t2, mid_lon_t2) %>% 
  mutate(time = "time 2") %>%
  rename(FIPS = FIPS_t2,
         n_unique_FIPS = n_unique_FIPS_t2,
         amen_rank = amen_rank_t2,
         lon = lon_t2,
         lat = lat_t2,
         avg_lon = avg_lon_t2,
         avg_lat = avg_lat_t2,
         mid_lon = mid_lon_t2,
         mid_lat = mid_lat_t2)

# Stack time 1 and time 2 into a long data.frame
merged_data_record_long <- rbind(merged_data_record_t1, merged_data_record_t2)

# Keep only distinct cases
merged_data_record_long <- distinct(merged_data_record_long)

# Summarize time 1 into tribe level
merged_data_tribe_t1 <- filter(merged_data_record_long, time == "time 1") %>%
  group_by(tribe) %>%
   summarize(n_unique_FIPS = mean(n_unique_FIPS, na.rm = TRUE),
             avg_amen_rank = round(mean(amen_rank, na.rm = TRUE), 2),
             mid_lon = mean(mid_lon, na.rm = TRUE),
             mid_lat = mean(mid_lat, na.rm = TRUE))

# Create time 1 subset at record level
merged_data_record_t2_r <- filter(merged_data_record_long, time == "time 2")

# Pair time 1 tribe level with time 2 record level data to link up the data
# Create ID number pair to visualize them as connected line
merged_data_tr1_r2 <- full_join(merged_data_tribe_t1, merged_data_record_t2_r, by = "tribe", suffix = c("_t1", "_t2")) %>%
  mutate(ID = row_number())

# Split them up again so we can reshape them into long format
merged_data_tr1 <- select(merged_data_tr1_r2, tribe, ID, avg_amen_rank, ends_with("_t1")) %>%
  rename(
    amen_rank = avg_amen_rank,
    lon = mid_lon_t1,
    lat = mid_lat_t1)

merged_data_r2 <- select(merged_data_tr1_r2, tribe, ID, FIPS, time, amen_rank, lon, lat, ends_with("_t2"))

# Stack time 1 tribe level data with time 2 record level data
# Use this data.frame for time 1 tribe to time 2 record connecting lines map
merged_data_tr1_r2_long <- bind_rows(merged_data_tr1, merged_data_r2) %>% 
  arrange(ID) %>%
  mutate(time = ifelse(!is.na(time), "time 2", "time 1"),
         n_unique_FIPS = ifelse(time == "time 1", n_unique_FIPS_t1, n_unique_FIPS_t2))


# --- Map Data --------------------------------------------

# Create wrapper fn to make map
map_tribe <- function(df, tribe_lst = NA, mid_t2 = FALSE, circle_size = c(3,5), show_legend = TRUE) {
  
  # Check if tribe_lst is presence, if it is, subset data to only those tribes
  if (is.na(tribe_lst[1])) {
    df <- df
  } else {
    df <- filter(df, tribe %in% tribe_lst)
  }
  
  # Make colour palette based on tribe
  pal <- colorFactor(topo.colors(20), domain = df$tribe)
  
  # Define colour based on tribe
  df$colour <- pal(df$tribe)
  
  # Identify the STATE FIPS where tribes live
  state_fp <- unique(str_sub(df$FIPS, 1, 2))
  # Subset shape files to only States where tribes live to only plot these
  sub_shape <- subset(shape, STATEFP %in% state_fp)
  
  # Make base map
  my_map <- leaflet(df) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = sub_shape, weight = .2, group = "County Boundary") # Map county boundaries
    
  
  # Use mid-point for time 2?
  if (mid_t2 == FALSE) {
    df <- df
  } else if (mid_t2 == TRUE) {
    df <- mutate(df, 
       lon = ifelse(time == "time 2", mid_lon_t2, lon),
       lat = ifelse(time == "time 2", mid_lat_t2, lat))
  }
    
  # Add connecting lines based on pair connection ID
    for (group in unique(df$ID)) {
      my_map <- addGeodesicPolylines(my_map, 
        lng= ~ lon,
        lat= ~ lat,
        data = df[df$ID == group,],
        steps = 50, # Set how smooth the lines is, more steps = smoother
        color= ~pal(tribe),
        weight = ~log(n_unique_FIPS), # set line weights to scale to no of unique FIPS (log scale)
        opacity = .5)
    }
    
    my_map <- my_map %>% addCircleMarkers(data = df, 
      lat = ~lat, 
      lng = ~lon, 
      popup = ~paste(
        '<strong>', tribe, '</strong>',
        '<br>Time: ', time,
        '<br>FIPS: ', FIPS,
        '<br>Amenity Rank: ', amen_rank),
      radius = ~scales::rescale(amen_rank, to = circle_size), # set circle radius to size according to amenity rank score, rescale using rescale
      stroke = "black", 
      weight = .8,
      fillColor =  ~pal(tribe),  # colour by tribes
      fillOpacity = .5)
    
    # Only show legend if <= 20 unique tribes in the data.frame
    if (show_legend == TRUE & length(unique(df$tribe)) <= 20) {
      my_map <- my_map %>% 
        addLegend("bottomright", pal = pal, values = ~tribe)
    } else {
      my_map
    }
    
    # Add checkbox to show or hide county boundary
    my_map <- my_map %>% addLayersControl(
    options = layersControlOptions(collapsed = FALSE),
    overlayGroups = c("County Boundary"))
    
    return(my_map)
} 

# --- Example Usage ---------------------------------------

# Call wrapper fn map_tribe to make maps

# Map 1 
# Map one tribe, map one tribe and use all-point for time 1 and 2
# Use merged_data_record_all_long as data.frame
map_tribe(merged_data_record_all_long, c("Arapaho"))
#all of the tribes (might crash?)
map_tribe(merged_data_record_all_long)

# Map 2
# Map one tribe, map one tribe and use mid-point for time 1 and time 2
# Use merged_data_tr1_r2_long as data.frame
map_tribe(merged_data_tr1_r2_long, c("Umpqua"), mid_t2 = TRUE)

# Map 3
# Map one tribe, map one tribe and use mid-point for time 1 and all-point for time 2
# Use merged_data_tr1_r2_long as data.frame
map_tribe(merged_data_tr1_r2_long, c("Arapaho"))


# Other Example

# Map one tribe
map_tribe(merged_data_tr1_r2_long, c("Absentee-Shawnee"))
map_tribe(merged_data_tr1_r2_long, "Piankeshaw")

# Map more than one tribe
map_tribe(merged_data_tr1_r2_long, c("Absentee-Shawnee", "Umpqua"))

# Map list of tribe from tribe in the time 1 and time 2 list
map_tribe(merged_data_tr1_r2_long, tribes_time1and2_lst$tribe)

# Map list of tribe from tribe in the time 1 and time 2 list
# Adjust circle size c(min, max)
map_tribe(merged_data_tr1_r2_long, tribes_time1and2_lst$tribe, circle_size = c(1,7))

# Map list of tribe from tribe in the time 1 and time 2 list and use mid-point for time 2
map_tribe(merged_data_tr1_r2_long, tribes_time1and2_lst$tribe, mid_t2 = TRUE)

# Map all tribe data
map_tribe(merged_data_tr1_r2_long)

# Mapp all tribe data and use mid-point for time 2
map_tribe(merged_data_tr1_r2_long, mid_t2 = TRUE)

# Randomly sample tribe data to map
map_tribe(merged_data_tr1_r2_long, sample(tribes_time1and2_lst$tribe, 5))
map_tribe(merged_data_tr1_r2_long, sample(tribes_time1and2_lst$tribe, 10))
map_tribe(merged_data_tr1_r2_long, sample(tribes_time1and2_lst$tribe, 20))

# Map all pairwise in time 1 and time 2 record level 
# Map one tribe
map_tribe(merged_data_record_all_long, c("Umpqua"))
map_tribe(merged_data_record_all_long, c("Absentee-Shawnee"))

# Map all pairwise in time 1 and time 2 record level 
# Map more than one tribe
map_tribe(merged_data_record_all_long, c("Absentee-Shawnee", "Umpqua"))

# Map all pairwise in time 1 and time 2 record level 
# from tribe in time 1 and time 2 list
map_tribe(merged_data_record_all_long, sample(tribes_time1and2_lst$tribe, 5))

# 10 tribe seems to be the maximum to plot before it crashes or run really slow
map_tribe(merged_data_record_all_long, sample(tribes_time1and2_lst$tribe, 10))

# --- Code Fragment ---------------------------------------