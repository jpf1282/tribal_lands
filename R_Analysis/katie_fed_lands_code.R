

# Append PAD-US government ownership data

## Import government .csv's and recalculate proportions so that they are between 0 - 1
# setwd("/Users/kathrynmcconnell/Dropbox (Yale_FES)/Tribal Lands/FedGov_Ben")
setwd("/Users/kathrynmcconnell/Dropbox (Yale_FES)/Tribal Lands/FedGov_Ben")

blm <- read_csv("BLM.csv") %>%
  mutate(blm_prop = BLM_Area / CountyArea)

dod <- read_csv("DOD.csv") %>%
  mutate(dod_prop = DOD_Area / CountyArea)

doe <- read_csv("DOE.csv") %>%
  mutate(doe_prop = DOE_Area / CountyArea)

nps <- read_csv("NPS.csv") %>%
  mutate(nps_prop = NPS_Area / CountyArea)

usfs <- read_csv("USFS.csv") %>%
  mutate(usfs_prop = USFSArea / CountyArea)

usfws <- read_csv("USFWS.csv") %>%
  mutate(usfws_prop = USFWS_Area / CountyArea)

# Join all government lands dataframes together and select only needed columns
govt <- full_join(blm, dod, by = "COUNTYNS") %>%
  select(STATEFP.x, COUNTYFP.x, COUNTYNS, NAME.x, blm_prop, dod_prop)  %>% # Select desired columns only
  full_join(doe, by = "COUNTYNS") %>% select(1:6, doe_prop) %>%
  full_join(nps, by = "COUNTYNS") %>% select(1:7, nps_prop) %>%
  full_join(usfs, by = "COUNTYNS") %>% select(1:8, usfs_prop) %>%
  full_join(usfws, by = "COUNTYNS") %>% select(1:9, usfws_prop) 

# Convert all NA's to 0's
govt$blm_prop[is.na(govt$blm_prop)] <- 0
govt$dod_prop[is.na(govt$dod_prop)] <- 0
govt$doe_prop[is.na(govt$doe_prop)] <- 0
govt$nps_prop[is.na(govt$nps_prop)] <- 0
govt$usfs_prop[is.na(govt$usfs_prop)] <- 0
govt$usfws_prop[is.na(govt$usfws_prop)] <- 0

# Create a new column for all federal government proportions combined (sums multiple columns for each row)
govt2 <- mutate(govt, 
                allfed_prop = rowSums(govt[,5:10]))

# Combine two columns, STATEFP.x and COUNTYFP.x, so that the new column is the join key 
govt3 <- govt2 %>% unite(FIPS, STATEFP.x, COUNTYFP.x, sep = "", remove = FALSE) 

## Join with counties at the records level for both t1 and t2
setwd("/Users/kathrynmcconnell/Documents/GitHub/tribal_lands2/R_Analysis")
records_level <- read_csv("change_scores_centroid_dist_records_level.csv")

# Join to t1 counties, rename columns so that they refer to t1, remove old columns (with old names)
govt_records_level_t1 <- left_join(records_level, govt3, by = c("FIPS_t1" = "FIPS")) %>% # join
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
  select(-(blm_prop:allfed_prop)) # remove old column names

# Join to t2 counties, rename columns so that they refer to t1, remove old columns (with old names)
govt_records_level_t2 <- left_join(records_level, govt3, by = c("FIPS_t2" = "FIPS")) %>%
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

# combine dataframes with t1 and t1 joins - this is the final, cleaned dataframe
govt_records_level_final <- 
  left_join(govt_records_level_t1, govt_records_level_t2) # don't specify a key here so multiple keys are used

# check to see how many columns were added in the join (should only be 10, the new mutated columns)
ncol(govt_records_level_final) - ncol(govt_records_level_t1)

# Confirm that both joined and unjoined rows are retained in the final dataframe
# Add joined rows and unjoined rows, compare to number of records in the original dataframe
# First code for t1, second code for t2
no_join1 <- anti_join(records_level, govt3, by = c("FIPS_t1" = "FIPS")) %>% nrow()
yes_join1 <- semi_join(records_level, govt3, by = c("FIPS_t1" = "FIPS")) %>% nrow()
no_join1 + yes_join1 == nrow(records_level) # should be TRUE if correct

no_join2 <- anti_join(records_level, govt3, by = c("FIPS_t2" = "FIPS")) %>% nrow()
yes_join2 <- semi_join(records_level, govt3, by = c("FIPS_t2" = "FIPS")) %>% nrow()
no_join2 + yes_join2 == nrow(records_level) # should be TRUE if correct

no_join_final <- anti_join(govt_records_level_t1, govt_records_level_t2) %>% nrow()
yes_join_final <- semi_join(govt_records_level_t1, govt_records_level_t2) %>% nrow()
no_join_final + yes_join_final == nrow(records_level) # should be TRUE if correct
