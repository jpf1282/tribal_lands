***
# <span style="color:red">[...tribes_level.csv]</span> Averages across all a tribe's counties
***
#### Description and Notes
 - <span style="color:green">Aggregated data at the tribe level (N=398), using **averages** if in multiple counties</span>
 - Data format: Wide
 - original name of filename in R code: `natural_amenity_with_change_scores_centriod_dist_tribes_level.csv` 

#### <span style="color:blue">Migration and Geo Variables</span>
 - **Tribe** Name of tribe
     + `tribe`
 - **Time:**  the historical records we have for the tribe. Some exist in T1 but not T2
     + `time`
 -  **Total number of locations at T1 and T2**: Count of unique counties at T1 and T2
     + `n_unique_FIPS_t1` , `n_unique_FIPS_t2`
 - **Number of Possible Pairwise Migration Records for a tribe** total number of pairwise migration possibilities for a tribe. e.g. All possible routes from T1 to T2.
     + `n_record`
 - **Average Location for tribe:** average (centroid) latitudes and longitudes at T1 and T2
     + `avg_lat_t1`, `avg_lat_t2`, `avg_lon_t1`, `avg_lon_t2`
 - **Average Location for tribe:** midpoint (alternative but similar avg. centroid measure) latitudes and longitudes at Time1 and Time2
     + `mid_lat_t1`, `mid_lat_t2`, `mid_lon_t1`, `mid_lon_t2`
 - **Average distance in KM between T1 and T2 for a tribe** using two centroids options above
     + `avg_dist` and `mid_dist`


#### <span style="color:blue">Variables We're Testing About County Change Characteristics</span>
 - **Average amenity rank**: average of all counties for a tribe, and Time2 minus Time1
     + scale from 0-7 
     + Time1 `avg_amen_rank_t1`, Time2: `avg_amen_rank_t2`, Change Score: `avg_amenity_change_score`
 -  **Average OIL production**: mean productivity b/t 2000-2011. Can convert to dollar amount if needed
     +  count variable beginning at zero
     +  `avg_oil_avg_t1`, `avg_oil_avg_t2`, `avg_oil_change_score`
 -  **Average GAS production**: mean productivity b/t 2000-2011. Can convert to dollar amount if needed
     +  count variable beginning at zero
     +  `avg_gas_avg_t1`, `avg_gas_avg_t2`, `avg_gas_change_score`
 - **Precipitation** 
     + Count variable? avg.
     + `avg_precip_t1`, `avg_precip_t2`, `avg_precip_change_score`  
 - **Climate Resilience Screening Index** 
     + Variable structure?
     + `avg_crsi_t1`, `avg_crsi_t2`, `avg_crsi_change_score`  
 - **EPA Risk** 
     + 0-1 scale?
     + `avg_risk_t1`, `avg_risk_t2`, `avg_risk_change_score`  
 - **EPA Governance** 
     + 0-1 scale?
     + `avg_governance_t1`, `avg_governance_t2`, `avg_governance_change_score`  
 - **EPA Built Environment** 
     + 0-1 scale?
     + `avg_built_env_t1`, `avg_built_env_t2`, `avg_built_env_change_score` 
 - **EPA Natural Environment** 
     + 0-1 scale?
     + `avg_natural_env_t1`, `avg_built_env_t2`, `avg_built_env_change_score`
 - **EPA Society** 
     + 0-1 scale?
     + `avg_society_env_t1`, `avg_society_env_t2`, `avg_societ_env_change_score`



***
# <span style="color:red">[...records_level.csv]</span> All single pairwise possibilities for a tribe
***
#### Description and Notes
 - <span style="color:green">All pairwise possibilities of county migration, so variables are discrete measures for a single county, unless otherwise noted. I include some of the avg variables from the tribe_level.csv above, but these should not be confused with the discrete (e.g. non-averaged) records in this dataset (N=62,109)</span>
 - Data format: Wide
 - original name of filename in R code: `natural_amenity_with_change_scores_centroid_dist_records_level.csv` 

#### <span style="color:blue">Migration and GEO Variables</span>
 - **Tribe** Name of tribe
     + `tribe`
 - **Time:**  the historical records we have for the tribe. Some exist in T1 but not T2
     + `time` 
     + values = `Time 1 and 2`, `Time 1 only`, `Time 2 only`
 - **County Location at Time 1 and Time2**
     + Time 1 county geo-locator = `FIPS_t1`
     + Time 2 county geo-locator = `FIPS_t2`
     + variables next to these have the actual name of county and state
 - **Lat/Long of Location at T1 and T2 for that row pair** the centroid for T1 and T2 for that single row pair
     + `lat_t1`, `lat_t2`,`lon_t1`, `lon_t2`
 - **Distance in KM between T1 and T2 for that row pair** for that single row pair
     + `dist`
 -  **Total number of locations at T1 and T2**: Count of unique counties at T1 and T2
     + `n_unique_FIPS_t1` , `n_unique_FIPS_t2`
 - **Number of Possible Pairwise Migration Records for a tribe** total number of pairwise migration possibilities for a tribe. e.g. All possible routes from T1 to T2.
     + `n_record`
 - **Average Location for tribe:** average (centroid) latitudes and longitudes at T1 and T2
     + `avg_lat_t1`, `avg_lat_t2`, `avg_lon_t1`, `avg_lon_t2`
     + these are averaged across all of the counties for that tribe, so are same as the tribes_level.csv dataset above
 - **Average Location for tribe:** midpoint (alternative but similar avg. centroid measure) latitudes and longitudes at Time1 and Time2
     + `mid_lat_t1`, `mid_lat_t2`, `mid_lon_t1`, `mid_lon_t2`
     + these are averaged across all of the counties for that tribe, so are same as the tribes_level.csv dataset above
 - **Average distance in KM between T1 and T2 for a tribe** using two centroids options above
     + `avg_dist` and `mid_dist`
     + these are averaged across all of the counties for that tribe, so are same as the tribes_level.csv dataset above

#### <span style="color:blue">Variables We're Testing About County Change Characteristics</span>
 - **Rural to Urban Scale for a county**
     + 1-9, with 1 being most urban and 9 being most rural
     + `Rural_Urban_Continuum_Code_2013_t1` (need to rename this variable)
 - **Amenity Rank raw score**: rank for single county at both T1 and T2 (*different than average in tribe_level.csv above*)
     + `amen_rank_t1` , `amen_rank_t2`
 - **Amenity Change Score for that row pair**: T2 *minus* T1 for that single row pair
     + `amen_change_score`
- **Average amenity rank for all a tribes counties**: average of all counties for a tribe, and Time2 minus Time1, so the same as the one in the other tribe_level.csv above
     + scale from 0-7 
     + Time1 `avg_amen_rank_t1`, Time2: `avg_amen_rank_t2`, Change Score: `avg_amenity_change_score`
 -  **Average OIL production for SINGLE COUNTY**: mean productivity for single county at Time 1 and Time 2, b/t 2000-2011. Can convert to dollar amount if needed. This is different than the tribe_level.csv above because it is not an average of all county avg Oil, but just single county average. 
     +  count variable beginning at zero
     +  `oil_avg_t1`, `oil_avg_t2`, `oil_change_score` *note the similarity in name of this variable to the one in the tribe_level.csv, but that it is not an average of all counties, but only average for this one county at T1 or T2*
-  **Average OIL production ACROSS ALL COUNTIES for this tribe**: mean productivity b/t 2000-2011. Can convert to dollar amount if needed. *same as variable in tribe_level.csv data above*
     +  count variable beginning at zero
     +  `avg_oil_avg_t1`, `avg_oil_avg_t2`, `avg_oil_change_score`  
 -  **Average GAS production for SINGLE COUNTY**: mean productivity for single county at Time 1 and Time 2, b/t 2000-2011. Can convert to dollar amount if needed. This is different than the tribe_level.csv above because it is not an average of all county avg gas, but just single county average. 
     +  count variable beginning at zero
     +  `gas_avg_t1`, `gas_avg_t2`, `gas_change_score`
 -  **Average GAS production ACROSS ALL COUNTIES for this tribe**: mean productivity b/t 2000-2011. Can convert to dollar amount if needed. *same as variable in tribe_level.csv data above*
     +  count variable beginning at zero
     +  `avg_gas_avg_t1`, `avg_gas_avg_t2`, `avg_gas_change_score`    
 - **Precipitation raw for that county** remember this is not the average, but the raw score for that individual county. For average across all the tribe's counties, see tribe_level.csv data above, 
     + Count variable? avg.
     + `precip_t1`, `precip_t2`, `precip_change_score`  
     + Like Oil/Gas in the bullet point above, I also included the aggregated average across all counties of a tribe (in the tribe_level.csv) into this dataset. These are `avg_precip_t1`, `avg_precip_t2` `avg_precip_change_score` and do not vary by county b/c they are averages across all counties.
 - **Climate Resilience Screening Index raw for that county** 
     + Variable structure?
     + `CRSI_t1`, `CRSI_t2`, `crsi_change_score`  (need to fix uppercase here)
     + Like the bullet point above, I also included the aggregated average across all counties of a tribe (in the tribe_level.csv) into this dataset. These are `avg_crsi_t1`, `avg_crsi_t2` , `avg_precip_change_score` and do not vary by county b/c they are averages across all counties.
 - **EPA Risk raw for that county** 
     + 0-1 scale?
     + `Risk_t1`,`Risk_t2`,`risk_change_score` (need to fix uppercase here)
     + Like the bullet point above, I also included the aggregated average across all counties of a tribe (in the tribe_level.csv) into this dataset. These are `avg_risk_t1`, `avg_risk_t2`, `avg_risk_change_score`   and do not vary by county b/c they are averages across all counties.
 - **EPA Governance raw for that county** 
     + 0-1 scale?
     + `Governance_t1`,`Governance_t2`,`governance_change_score` (need to fix uppercase here)
     + Like the bullet point above, I also included the aggregated average across all counties of a tribe (in the tribe_level.csv) into this dataset. These are `avg_governance_t1`, `avg_governance_t2`, `avg_governance_change_score`   and do not vary by county b/c they are averages across all counties.
 - **EPA Built Environment raw for that county** 
     + 0-1 scale?
     + `Built Environment_t1`,`Built Environment_t2`,`built_env_change_score` (need to fix uppercase and inconsistent spelling here)
     + Like the bullet point above, I also included the aggregated average across all counties of a tribe (in the tribe_level.csv) into this dataset. These are `avg_built_env_t1`, `avg_built_env_t2`, `avg_built_env_change_score` and do not vary by county b/c they are averages across all counties.
 - **EPA Natural Environment raw for that county** 
     + 0-1 scale?
     + `Natural Environment_t1`,`Natural Environment_t2`,`natural_env_change_score` (need to fix uppercase and inconsistent spelling here)
     + Like the bullet point above, I also included the aggregated average across all counties of a tribe (in the tribe_level.csv) into this dataset. These are `avg_natural_env_t1`, `avg_natural_env_t2`, `avg_natural_env_change_score` and do not vary by county b/c they are averages across all counties.
 - **EPA Society raw for that county** 
     + 0-1 scale?
     + `Society_t1`,`Society_t2`,`society_change_score` (need to fix uppercase and inconsistent spelling here)
     + Like the bullet point above, I also included the aggregated average across all counties of a tribe (in the tribe_level.csv) into this dataset. These are `avg_society_t1`, `avg_society_t2`, `avg_society_change_score` and do not vary by county b/c they are averages across all counties.