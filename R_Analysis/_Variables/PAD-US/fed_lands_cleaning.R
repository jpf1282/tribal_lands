

# Append PAD-US government ownership data

## Import government .csv's and recalculate proportions so that they are between 0 - 1

#KATIE wd
setwd("/Users/kathrynmcconnell/Dropbox (Yale_FES)/Tribal Lands/FedGov_Ben")
#JUSTIN wd
#setwd("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Variables/PAD-US/")

blm <- read_csv("BLM.csv") %>%
  mutate(blm_prop = BLM_Area / CountyArea) %>%
  mutate(statefp_blm = STATEFP) %>%
  mutate(countyfp_blm = COUNTYFP)

dod <- read_csv("DOD.csv") %>%
  mutate(dod_prop = DOD_Area / CountyArea) %>%
  mutate(statefp_dod = STATEFP) %>%
  mutate(countyfp_dod = COUNTYFP)

doe <- read_csv("DOE.csv") %>%
  mutate(doe_prop = DOE_Area / CountyArea) %>%
  mutate(statefp_doe = STATEFP) %>%
  mutate(countyfp_doe = COUNTYFP)

nps <- read_csv("NPS.csv") %>%
  mutate(nps_prop = NPS_Area / CountyArea) %>%
  mutate(statefp_nps = STATEFP) %>%
  mutate(countyfp_nps = COUNTYFP)

usfs <- read_csv("USFS.csv") %>%
  mutate(usfs_prop = USFSArea / CountyArea) %>%
  mutate(statefp_usfs = STATEFP) %>%
  mutate(countyfp_usfs = COUNTYFP)

usfws <- read_csv("USFWS.csv") %>%
  mutate(usfws_prop = USFWS_Area / CountyArea) %>%
  mutate(statefp_usfws = STATEFP) %>%
  mutate(countyfp_usfws = COUNTYFP)

# Are any individual dataframes missing FP values? No (should all be zero)
sum(is.na(blm$STATEFP))
sum(is.na(dod$STATEFP))
sum(is.na(doe$STATEFP))
sum(is.na(nps$STATEFP))
sum(is.na(usfs$STATEFP))
sum(is.na(usfws$STATEFP))

# Join all government lands dataframes together 
govt <- full_join(blm, dod, by = "COUNTYNS") %>%
  full_join(doe, by = "COUNTYNS") %>% 
  full_join(nps, by = "COUNTYNS") %>% 
  full_join(usfs, by = "COUNTYNS") %>% 
  full_join(usfws, by = "COUNTYNS") 

# Combine all separate State FIPS columns into one
govt$STATEFP.x[is.na(govt$STATEFP.x)] <- govt$statefp_dod[is.na(govt$STATEFP.x)] 
govt$STATEFP.x[is.na(govt$STATEFP.x)] <- govt$statefp_doe[is.na(govt$STATEFP.x)]
govt$STATEFP.x[is.na(govt$STATEFP.x)] <- govt$statefp_nps[is.na(govt$STATEFP.x)] 
govt$STATEFP.x[is.na(govt$STATEFP.x)] <- govt$statefp_usfs[is.na(govt$STATEFP.x)]       
govt$STATEFP.x[is.na(govt$STATEFP.x)] <- govt$statefp_usfws[is.na(govt$STATEFP.x)] 

# Check to see if there are any missing values (should equal zero)
sum(is.na(govt$STATEFP.x))

# Combine all separate County FIPS columns into one
govt$COUNTYFP.x[is.na(govt$COUNTYFP.x)] <- govt$countyfp_dod[is.na(govt$COUNTYFP.x)] 
govt$COUNTYFP.x[is.na(govt$COUNTYFP.x)] <- govt$countyfp_doe[is.na(govt$COUNTYFP.x)] 
govt$COUNTYFP.x[is.na(govt$COUNTYFP.x)] <- govt$countyfp_nps[is.na(govt$COUNTYFP.x)] 
govt$COUNTYFP.x[is.na(govt$COUNTYFP.x)] <- govt$countyfp_usfs[is.na(govt$COUNTYFP.x)] 
govt$COUNTYFP.x[is.na(govt$COUNTYFP.x)] <- govt$countyfp_usfws[is.na(govt$COUNTYFP.x)] 

# Check to see if there are any missing values (should equal zero)
sum(is.na(govt$COUNTYFP.x))

# Select only the columns that are needed
govt2 <- select(govt, 
                STATEFP.x, 
                COUNTYFP.x, 
                COUNTYNS, 
                NAME.x, 
                blm_prop, 
                dod_prop,
                doe_prop,
                nps_prop,
                usfs_prop,
                usfws_prop
)

# Convert all NA's to 0's
govt2$blm_prop[is.na(govt2$blm_prop)] <- 0
govt2$dod_prop[is.na(govt2$dod_prop)] <- 0
govt2$doe_prop[is.na(govt2$doe_prop)] <- 0
govt2$nps_prop[is.na(govt2$nps_prop)] <- 0
govt2$usfs_prop[is.na(govt2$usfs_prop)] <- 0
govt2$usfws_prop[is.na(govt2$usfws_prop)] <- 0

# Create a new column for all federal government proportions combined (sums multiple columns for each row)
govt3 <- mutate(govt2, 
                allfed_prop = rowSums(govt2[,5:10]))

# Combine two columns, STATEFP.x and COUNTYFP.x, so that the new column is the join key 
govt4 <- govt3 %>% unite(FIPS, STATEFP.x, COUNTYFP.x, sep = "", remove = FALSE) 

# # Clean a bit more to prepare for master join. RENAMING columns
# govt4 <- rename(govt4,
#                 pa_all = allfed_prop,
#                 pa_blm = blm_prop,
#                 pa_dod = dod_prop,
#                 pa_doe = doe_prop,
#                 pa_nps = nps_prop,
#                 pa_usfs = usfs_prop,
#                 pa_usfws = usfws_prop)

# Keep only relevant variables
# govt3 <- select(govt3, -c(COUNTYFP.x,COUNTYNS,STATEFP.x,NAME.x))
# write_csv(govt3, "~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Variables/PAD-US/protected_areas_master.csv")


#
# ## Join with counties at the records level for both t1 and t2
#setwd("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/") # Justin's
setwd("/Users/kathrynmcconnell/Documents/GitHub/tribal_lands2/R_Analysis") # Katie's
records_level <- read_csv("change_scores_centroid_dist_records_level.csv")

# Join to t1 counties, rename columns so that they refer to t1, remove old columns (with old names)
govt_records_level_t1 <- left_join(records_level, govt4, by = c("FIPS_t1" = "FIPS")) %>% # join
  mutate(StateFP_t1 = STATEFP.x, # rename columns
         CountyFP_t1 = COUNTYFP.x,
         CountyNS_t1 = COUNTYNS,
         blm_prop_t1 = blm_prop,
         dod_prop_t1 = dod_prop,
         doe_prop_t1 = doe_prop,
         nps_prop_t1 = nps_prop,
         usfs_prop_t1 = usfs_prop,
         usfws_prop_t1 = usfws_prop,
         allfed_prop_t1 = allfed_prop) %>%
  select(-(STATEFP.x:allfed_prop)) # remove old column names

# Join federal lands to t2 counties, rename columns so that they refer to t2, remove old columns (with old names)
# This is the final dataframe (after replacing NA's with zeros below)
govt_records_level_t1t2 <- left_join(govt_records_level_t1, govt4, by = c("FIPS_t2" = "FIPS")) %>%
  mutate(StateFP_t2 = STATEFP.x, # rename columns
         CountyFP_t2 = COUNTYFP.x,
         CountyNS_t2 = COUNTYNS,
         blm_prop_t2 = blm_prop,
         dod_prop_t2 = dod_prop,
         doe_prop_t2 = doe_prop,
         nps_prop_t2 = nps_prop,
         usfs_prop_t2 = usfs_prop,
         usfws_prop_t2 = usfws_prop,
         allfed_prop_t2 = allfed_prop) %>%
  select(-(STATEFP.x:allfed_prop)) # remove old column names

# check to see how many columns were added in the join (should only be 10, the new mutated columns)
ncol(govt_records_level_t1t2) - ncol(govt_records_level_t1)

# Convert all federal land NA's to 0's (we know that absence of data for these columns means no govt presence)
# time 1
govt_records_level_t1t2$blm_prop_t1[is.na(govt_records_level_t1t2$blm_prop_t1)] <- 0
govt_records_level_t1t2$dod_prop_t1[is.na(govt_records_level_t1t2$dod_prop_t1)] <- 0
govt_records_level_t1t2$doe_prop_t1[is.na(govt_records_level_t1t2$doe_prop_t1)] <- 0
govt_records_level_t1t2$nps_prop_t1[is.na(govt_records_level_t1t2$nps_prop_t1)] <- 0
govt_records_level_t1t2$usfs_prop_t1[is.na(govt_records_level_t1t2$usfs_prop_t1)] <- 0
govt_records_level_t1t2$usfws_prop_t1[is.na(govt_records_level_t1t2$usfws_prop_t1)] <- 0

# time 2
govt_records_level_t1t2$blm_prop_t2[is.na(govt_records_level_t1t2$blm_prop_t2)] <- 0
govt_records_level_t1t2$dod_prop_t2[is.na(govt_records_level_t1t2$dod_prop_t2)] <- 0
govt_records_level_t1t2$doe_prop_t2[is.na(govt_records_level_t1t2$doe_prop_t2)] <- 0
govt_records_level_t1t2$nps_prop_t2[is.na(govt_records_level_t1t2$nps_prop_t2)] <- 0
govt_records_level_t1t2$usfs_prop_t2[is.na(govt_records_level_t1t2$usfs_prop_t2)] <- 0
govt_records_level_t1t2$usfws_prop_t2[is.na(govt_records_level_t1t2$usfws_prop_t2)] <- 0






