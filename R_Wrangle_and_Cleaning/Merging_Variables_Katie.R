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

# Data comes from an .RData file that Justin created (documentation available in 'data wrangle change score centroid and map data.R`)
load("/Users/kathrynmcconnell/Documents/GitHub/tribal_lands2/R_Analysis/processed_data.RData")

# Then select distinct records only
data_long <- distinct(merged_data_record_all_long, tribe, time, FIPS, .keep_all = TRUE)

# Change FIPS from character to integer 
data_long$FIPS <- as.numeric(data_long$FIPS)

# Join new variables to all tribe records (for some reason adds around ten records) - update this in github
# Adds nine more rows, not sure which
main_join1 <- left_join(data_long, precip, c("FIPS" = "FIPS")) %>% # Join precip data
  left_join(climate, c("FIPS" = "FIPS")) %>% # Join climate data 
  mutate(Precip = MeanAnnualPrecipitation_in) %>% # Shorten variable names
  select(-(FIPS_manual)) %>% # Remove any extraneous columns from added datasets that we don't want
  select(-(MeanAnnualPrecipitation_in))

# write to .csv
#setwd("/Users/kathrynmcconnell/Documents/GitHub/tribal_lands2/Clean_Tribe_Files_for_Analysis")
#write.csv(main_join1, "Tribes_merged_2.24.19.csv")

# Select only tribes with time 1 and time 2 data 
data_t1and2_long <- filter(main_join1, tribe %in% tribes_time1and2_lst$tribe)

# Change from character to numeric
data_t1and2_long$FIPS <- as.numeric(data_t1and2_long$FIPS) 

# Confirm that there are only two records per tribe, result should be zero
t1and2_lst <- group_by(data_t1and2_long, tribe, time) %>% 
  count() %>%
  ungroup() %>%
  group_by(tribe) %>%
  count() %>%
  filter(nn < 2)

nrow(t1and2_lst)


# Exploratory Analysis ----------------------------------------------------

### All tribes, including those missing values at t1 or t2
# Precipitation differences between t1 and t2 
ggplot(main_join1, aes(Precip, fill = time)) +
  geom_histogram(position = "dodge", binwidth = 200) +
  theme_minimal() +
  ylab("Count of Counties with Tribes Present") +
  xlab("Mean Annual Precipitation (in)") +
  facet_wrap(~ time, nrow = 2)

ggplot(main_join1, aes(time, Precip, colour = time)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 1/10, varwidth = TRUE) +
  theme_minimal() +
  ylab("Mean Annual Precipitation (in)") +
  ggtitle("Counties with Tribes Present at Time 1 & 2", "All Tribes")

# EPA Risk
ggplot(main_join1, aes(Risk, fill = time)) +
  geom_histogram(position = "dodge", binwidth = .10) +
  theme_minimal() +
  ylab("Count of Counties with Tribes Present") +
  xlab("EPA Risk Scale") +
  facet_wrap(~ time, nrow = 2)

ggplot(main_join1, aes(time, Risk, colour = time)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 1/10, varwidth = TRUE) +
  theme_minimal() +
  ylab("EPA Risk Scale") +
  ggtitle("Counties with Tribes Present at Time 1 & 2", "All Tribes")

### Only tribes with t1 and t2 values
ggplot(data_t1and2_long, aes(Precip, fill = time)) +
  geom_histogram(position = "dodge", binwidth = 200) +
  theme_minimal() +
  ylab("Count of Counties with Tribes Present") +
  xlab("Mean Precipitation (in)") +
  facet_wrap(~ time, nrow = 2)

ggplot(data_t1and2_long, aes(time, Precip, colour = time)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 1/10, varwidth = TRUE) +
  theme_minimal() +
  ylab("Mean Annual Precipitation (in)") +
  ggtitle("Counties with Tribes Present", "Only Tribes with T1 & T2")

# EPA Risk 
ggplot(data_t1and2_long, aes(Risk, fill = time)) +
  geom_histogram(position = "dodge", binwidth = .05) +
  theme_minimal() +
  ylab("Count of Counties with Tribes Present") +
  xlab("EPA Risk Scale") +
  facet_wrap(~ time, nrow = 2)

ggplot(data_t1and2_long, aes(time, Risk, colour = time)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 1/10, varwidth = TRUE) +
  theme_minimal() +
  ylab("EPA Risk Scale") +
  ggtitle("Counties with Tribes Present at Time 1 & 2", "Only Tribes with T1 & T2")


# Analysis ----------------------------------------------------------------


# Are data normally distributed? Doesn't immediately look like it
library(ggpubr)

# QQ plots plot sample observations against a normal mean (theoretical)
# None look normal, but for precipitation the main differences are in the tails
ggqqplot(main_join1$Risk) + ggtitle("EPA Risk, All Tribes")
ggqqplot(data_t1and2_long$Risk) + ggtitle("EPA Risk, Only Tribes with T1 and T2")
ggqqplot(main_join1$Precip) + ggtitle("Precipitation, All Tribes")
ggqqplot(data_t1and2_long$Precip) + ggtitle("Precipitation, Only Tribes with T1 and T2")

# Density of precipitation for all tribes
ggdensity(main_join1$Precip, 
          main = "Density plot of precipitation (all tribes)",
          xlab = "Precipitation (in)")

# Density of precipitation for tribes with only t1 and t2
ggdensity(data_t1and2_long$Precip, 
          main = "Density plot of precipitation (tribes with t1 and t2)",
          xlab = "Precipitation (in)")

# Density of EPA risk for all tribes
ggdensity(main_join1$Risk, 
          main = "Density plot of EPA risk (all tribes)",
          xlab = "Risk")

# Density of EPA risk for tribes with t1 and t2
ggdensity(data_t1and2_long$Risk, 
          main = "Density plot of EPA risk (tribes with t1 and t2)",
          xlab = "Risk")


### T-tests for precipitation
# T-test for all tribe-counties and precipitation
precip_all_ttest <- t.test(main_join1$Precip[main_join1$time == "time 1"], 
                           main_join1$Precip[main_join1$time == "time 2"])

precip_all_ttest

# T-test for only t1 and t2 tribe-counties and precipitation
precip_t1t2_ttest <- t.test(data_t1and2_long$Precip[data_t1and2_long$time == "time 1"], 
                           data_t1and2_long$Precip[data_t1and2_long$time == "time 2"])

precip_all_ttest

# T-test for all tribe-counties and EPA risk
risk_all_ttest <- t.test(main_join1$Risk[main_join1$time == "time 1"], 
                           main_join1$Risk[main_join1$time == "time 2"])

risk_all_ttest

# T-test for only t1 and t2 tribe-counties and EPA risk
risk_t1t2_ttest <- t.test(data_t1and2_long$Risk[data_t1and2_long$time == "time 1"], 
                            data_t1and2_long$Risk[data_t1and2_long$time == "time 2"])

risk_t1t2_ttest


