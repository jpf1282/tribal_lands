# Add FIPS code to EPA data

# Load library
library(usmap)
library(readr)
library(dplyr)

# --- Read in EPA file ------------------------------------

# Import UTF8 file instead of original
# I manually resave the orginal as UTF8 encoded CSV in Excel
epa_0 <- read_csv("EPA_FIPS_UTF8.csv")

# --- Data Check ------------------------------------------

# Single example
fips(state = "Alabama", county = "Autauga County")
fips(state = "California", county = "San Bernardino")

# Example with actual data
fips(state = epa_0$State[1], county = epa_0$County[1])
fips(state = epa_0$State[1], county = epa_0$County_with_county_appended[1])

# Check for duplicates
epa_0 %>% 
  group_by(State, County_with_county_appended) %>% 
  count() %>%
  arrange(-n) %>%
  filter(n > 1)

# --- Custom Fn -------------------------------------------

# Custom fips function that output the error message rather than stop the fn
fips_r <- function (state, county = c()) 
{
    if (missing(state)) {
        stop("`state` must be specified. Use full name (e.g. \"Alabama\") or two-letter abbreviation (e.g. \"AL\").")
    }
    state_ <- tolower(state)
    county_ <- tolower(county)
    if (length(county_) == 0) {
        df <- utils::read.csv(system.file("extdata", "state_fips.csv", 
            package = "usmap"))
        abbr <- tolower(df$abbr)
        full <- tolower(df$full)
        fips2 <- c(df$fips, df$fips)
        result <- fips2[match(state_, c(abbr, full))]
        formatted_result <- sprintf("%02d", result)
        formatted_result[formatted_result == "NA"] <- NA
        formatted_result
    }
    else {
        if (length(state_) > 1) {
            stop("`county` parameter cannot be used with multiple states.")
        }
        df <- utils::read.csv(system.file("extdata", "county_fips.csv", 
            package = "usmap"))
        name <- tolower(df$county)
        state_abbr <- tolower(df$abbr)
        state_full <- tolower(df$full)
        result <- c()
        for (county_i in county_) {
            result <- c(result, df$fips[which((name %in% county_i | 
                name %in% paste(county_i, "county")) & (state_abbr %in% 
                state_ | state_full %in% state_))])
        }
        if (length(result) == 0) {
            if (length(county) == 1) {
              # Custom Return
              return(paste0(county, " is not a valid county in ", state, ".\n"))
                # stop(paste0(county, " is not a valid county in ", 
                #   state, ".\n"))
            }
            else {
              # Custom Return
              return(paste0(county, " are not valid counties in ", state, ".\n"))
                # stop(paste0(county, " are not valid counties in ", 
                #   state, ".\n"))
            }
        }
        else {
            sprintf("%05d", result)
        }
    }
}

# --- Add FIPS code to EPA data ---------------------------

# Find FIPS code in one batch with rowise DO and custom fips fn
fips_list <- epa_0 %>% 
  rowwise() %>%
  do(data.frame(FIPS = fips_r(state = .$State, county = .$County)))

# Add FIPS code to main data
epa_1 <- bind_cols(epa_0, fips_list) %>%
  arrange(desc(FIPS))

# Write file out
write_csv(epa_1, "EPA_withFIPS_UTF8.csv")
 