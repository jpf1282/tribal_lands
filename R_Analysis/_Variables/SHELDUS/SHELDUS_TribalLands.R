library(tidyverse)
library(dplyr)

# setwd("/Users/kathrynmcconnell/Documents/GitHub/tribal_lands2/R_Analysis/_Variables/SHELDUS")
setwd("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Variables/SHELDUS/")


# Load data
shel_a <- read_csv("UID19012f_AGG_A.csv")

# Change column names so there are no spaces in them
colnames(shel_a)[colnames(shel_a)=="CropDmg(ADJ 2017)"] <- "CropDmg_adj17"
colnames(shel_a)[colnames(shel_a)=="CropDmgPerCapita(ADJ 2017)"] <- "CropDmg_percap_adj17"
colnames(shel_a)[colnames(shel_a)=="PropertyDmg(ADJ 2017)"] <- "PropertyDmg_adj17"
colnames(shel_a)[colnames(shel_a)=="PropertyDmgPerCapita(ADJ 2017)"] <- "PropertyDmg_percap_adj17"
colnames(shel_a)[colnames(shel_a)=="State Name"] <- "StateName"
colnames(shel_a)[colnames(shel_a)=="County Name"] <- "CountyName"
colnames(shel_a)[colnames(shel_a)=="County FIPS"] <- "CountyFIPS"

# Remove the ' ' from county FIPS for future joins
shel2 <- separate(shel_a, CountyFIPS, into = c("extra", "CountyFIPS"), sep = 1) %>%
  separate(CountyFIPS, into = c("CountyFIPS", "extra2"), sep = -1) %>%
  select(-("extra"), -("extra2"))

# Group so that each row is one county with SHELDUS variables of interest summed
shel3 <- group_by(shel2, `CountyFIPS`) %>%
  summarise(unused_CropDmg_med = median(CropDmg),
            shel_crop = median(CropDmg_adj17),
            shel_crop_pc = median(CropDmg_percap_adj17),
            unused_PropertyDmg_med = median(PropertyDmg),
            shel_prop = median(PropertyDmg_adj17),
            shel_prop_pc = median(PropertyDmg_percap_adj17),
            shel_injur = median(Injuries),
            shel_injur_pc = median(InjuriesPerCapita),
            shel_fatal = median(Fatalities),
            shel_fatal_pc = median(FatalitiesPerCapita),
            shel_dura = median(Duration_Days),
            shel_yr_total = n())

# # Plotting
# ggplot(data = shel3, aes(x = "", y = shel_crop) +
#   geom_boxplot(alpha = .3) +
#   ylab("Crop Damage in $") +
#   xlab(" ") +
#   ggtitle("County-Level Crop Damage") +
#   theme_bw() 
# 
# # Property Damage 
# ggplot(data = shel3, aes(x = "", y = shel_prop)) +
#   geom_boxplot(alpha = .3) +
#   ylab("Property Damage in $") +
#   xlab(" ") +
#   ggtitle("County-Level Property Damage") +
#   theme_bw() 
# 
# # Duration Days
# ggplot(data = shel3, aes(x = "", y = shel_dura)) +
#   geom_boxplot(alpha = .3) +
#   ylab("Number of Hazard Days") +
#   xlab(" ") +
#   ggtitle("County-Level Counts of Hazard Days") +
#   theme_bw() 

# CREATE FINAL .CSV THAT WE'LL USE TO MERGE
shel4 <- shel3 %>% select(CountyFIPS, shel_crop, shel_crop_pc, shel_prop, shel_prop_pc,
                          shel_injur, shel_injur_pc, shel_fatal, shel_fatal_pc,
                          shel_dura, shel_yr_total)
write_csv(shel4, "SHELDUS_to_merge_final.csv")
