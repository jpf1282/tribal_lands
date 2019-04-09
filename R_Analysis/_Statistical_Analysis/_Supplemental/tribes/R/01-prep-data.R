fname = file.path(data_path, "change_scores_centroid_dist_records_level.csv")
df = read_csv(fname)

# select only target variables
if (target == "gas")
        yvars = c("gas_avg_t1", "gas_avg_t2", "gas_change_score")
if (target == "risk")
        yvars = c("Risk_t1", "Risk_t2", "risk_change_score")
if (target == "precipitation")
        yvars = c("precip_t1", "precip_t2", "precip_change_score")
if (target == "amen_rank")
        yvars = c("amen_rank_t1", "amen_rank_t2", "amenity_change_score")
df = df %>% select(tribe, yvars)

# rename the variables:
#       gas_avg_t1 -> t1, gas_avg_t1 -> t2, gas_change_score -> change_score,
#       Risk_t1 -> t1, Risk_t2 -> t2, risk_change_score -> change_score,
#       ...
# doing this will allow us to reuse the same scripts for different targets, 
# keeping the codebase lean.
names(df) = gsub(".*_(t1|t2)", "\\1", names(df))
names(df) = gsub(".*_(change_score)", "\\1", names(df))
# glimpse(df)

# --- prep data --- #

# drop missings
cat(target, "has", is.na(df$t1) %>% sum(), "NAs at t1 and", 
    is.na(df$t2) %>% sum(), "NAs at t2. Drop them!\n\n")
df = df %>% filter(!is.na(t1), !is.na(t2))

# change to long format
df_long = to_long(df)
# glimpse(df_long)

if (target == "gas") {
        # separate values < 1000 from values >= 1000
        df_tiny = df %>% filter(t1 < 1000 | t2 < 1000) 
        df_main = df %>% filter(t1 >= 1000, t2 >= 1000) 
        stopifnot(nrow(df_tiny) + nrow(df_main) == nrow(df))
        
        # change to long format
        df_tiny_long = to_long(df_tiny)
        df_main_long = to_long(df_main)

        # glimpse(df_tiny_long)
        # glimpse(df_main_long)        
}
