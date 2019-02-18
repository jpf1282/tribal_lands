library(readr)
library(tidyr)
library(knitr)
library(DT)
library(ggbeeswarm)
library(dplyr)
library(stringr)
library(scales)
library(broom)
library(ggplot2)



# Import variables to be joined to tribes ---------------------------------

# Import variables to be joined (climate and precipitation)
setwd("/Users/kathrynmcconnell/Documents/GitHub/tribal_lands2/R_Analysis/_Variables")
climate <- read_csv("EPA_CSRI_Index_Final.csv") %>% select(FIPS, FIPS_manual, Risk, CRSI)
precip <- read_csv("MeanAnnualPrecipitation.csv") %>% select(FIPS, MeanAnnualPrecipitation_in)

# For counties that were missing automatic FIPS designations use manual FIPS designation instead
climate[1:126,]$FIPS <- climate[1:126,]$FIPS_manual

# Change FIPS from character to integer for future joins
climate$FIPS <- as.numeric(climate$FIPS) 
precip$FIPS <- as.numeric(precip$FIPS)


# Processing Justin’s tribal data and completing the join -----------------

# Data comes from an .RData file that is loaded into the environment
# Then select distinct records only
data_long <- distinct(merged_data_record_all_long, tribe, time, FIPS, .keep_all = TRUE)

# Select only tribes with time 1 and time 2 data 
data_t1and2_long <- filter(data_long, tribe %in% tribes_time1and2_lst$tribe)

# Confirm that there are only two records per tribe, result should be zero
t1and2_lst <- group_by(data_t1and2_long, tribe, time) %>% 
  count() %>%
  ungroup() %>%
  group_by(tribe) %>%
  count() %>%
  filter(nn < 2)

nrow(t1and2_lst)

# Change FIPS from character to integer 
data_t1and2_long$FIPS <- as.numeric(data_t1and2_long$FIPS) 

# Combining tribes with additional variables - for some reason this adds in another ~5 rows, not sure why
main_join <- left_join(data_t1and2_long, precip, c("FIPS" = "FIPS")) %>% # Join precip data
  left_join(climate, c("FIPS" = "FIPS")) %>% # Join climate data 
  select(-(FIPS_manual)) # Remove any extraneous columns from added datasets that we don't want

# write to .csv
#write.csv(main_join, "Tribes_merged_2.18.19.csv")








