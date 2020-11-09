
library(tidyverse)
library(dplyr)
library(sf)

### Examine geographic distribution of tribes which do not have any federally recognized land at T2 ###

# Filter different tribe-time configurations ------------------------------

# Read in all tribe records
all_tribes <- read_csv("/Users/kathrynmcconnell/Documents/GitHub/tribal_lands/R_Analysis/_Variables/tribes.csv")

# All tribes recorded at T1 (370 tribes)
tribes_t1 <- filter(all_tribes, time == "time1") %>%
  select(tribe) %>%
  unique()

# All tribes recorded at T2 (212 tribes)
tribes_t2 <- filter(all_tribes, time == "time2") %>%
  select(tribe) %>%
  unique()

# Which tribes were recorded at T1 but had no federally recognized land base at T2 (186 tribes)
tribes_t1_only <- anti_join(tribes_t1, tribes_t2)

# Which tribes were not recorded at T1 but did have a federally recognized land base at T2 (28 tribes)
tribes_t2_only <- anti_join(tribes_t2, tribes_t1)


# Map T1 only tribes ------------------------------------------------------

# Select all records for tribes in T1 only
tribes_t1_only2 <- filter(all_tribes, tribe %in% as.vector(tribes_t1_only$tribe))
  
# Double check to make sure the filter worked properly
unique(tribes_t1_only2$tribe) == tribes_t1_only$tribe

# What states were T1 only tribes in?
T1_tribes_per_state <- tribes_t1_only2 %>%
  select(tribe, state_from_fips) %>%
  unique() %>%
  group_by(state_from_fips) %>%
  summarise(Total_T1only_Tribes = n())

# Read in all counties
counties <- st_read("/Users/kathrynmcconnell/Documents/GitHub/tribal_lands/R_Analysis/x_County_Shapefiles/cb_2017_us_county_20m.shp") %>%
  mutate(GEOID = as.character(GEOID) %>% as.numeric()) %>% # match FIPS column
  mutate(STATEFP = as.character(STATEFP)) %>%
  filter(STATEFP != "02" &
         STATEFP != "15" &
           STATEFP != "72") # remove AK, HI, PR geometries for easier mapping
  
str(counties)
str(tribes_t1_only2)
    
# Select only counties with presence of T1 tribe
counties_t1_tribes <- filter(counties, GEOID %in% tribes_t1_only2$FIPS)

# Plot
plot(st_geometry(counties), col = "grey")
plot(st_geometry(counties_t1_tribes), col = "blue", add = TRUE)









