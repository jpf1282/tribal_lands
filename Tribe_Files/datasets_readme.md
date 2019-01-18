***
# amenity_tribes_level.csv
***
#### Description and Notes
 - Aggregated data at the tribe level (N=398), using averages if in multiple counties
 - Data format: Wide
 - original name of filename in R code: `natural_amenity_with_change_scores_centriod_dist_tribes_level.csv` 

#### Variables
 - **Time:**  the historical records we have for the tribe. Some exist in T1 but not T2
 - **Average amenity rank for tribe**: average of all counties at T1 and T2 (`avg_amen_rank_t1`...)
 - **Average Locations for tribe:** average (centroid) latitudes and longitudes at T1 and T2 (`avg_lat`...)
 - **Average Locations for tribe:** midpoint (alternative but similar centroid measure) latitudes and longitudes at Time1 and Time2 (`mid_lat`...)
 - **Average distance in KM between T1 and T2 for a tribe** using two centroids options above (`avg_dist` and `mid_dist`)
 - **Total number of locations at T1 versus T2**: Count of unique counties at T1 and T2 (`n_unique_FIPS_t1` & `n_unique_FIPS_t2`)
 - **Number of Possible Pairwise Migration Records for a tribe** total number of pairwise migration possibilities for a tribe. All possible routes from T1 to T2. (`n_record`)

***
# amenity_records_level.csv
***
#### Description and Notes
 - All pairwise possibilities of county migration (N=62,109)
 - Data format: Wide
 - original name of filename in R code: `natural_amenity_with_change_scores_centroid_dist_records_level.csv` 

#### Variables
 - **Time:**  the historical records we have for the tribe. Some exist in T1 but not T2
 - **Raw Amenity Rank**: rank for pairwise county at both T1 and T1 (`amen_rank_t1` & `amen_rank_t2`)
 - **Amenity Change Score for that row pair**: T1 *minus* T2 for that single row pair
 - **Average Amenity Rank for tribe**: aggregated measure for T1 and T2 for that tribe, same as one in tribe level dataset `amenity_tribes_level.csv` 
 - **Average Amenity Change Score for a tribe** avg at T1 *minus* avg at T2 (maybe we should include this in the tribe_level dataset above) (`avg_amenity_change_score`)
 - **Location T1 and T2 for that row pair**:  (`FIPS_t1` and `FIPS_t2`)
 - **Name/State of Counties:**  (`fips.name_t1` and `fips.name_t2` and `state` as well)
 - **Lat/Long of Location at T1 and T2 for that row pair** the centroid for T1 and T2 for that single row pair (`lat_t1` & `lon_t2`)
 - **Distance in KM between T1 and T2 for that row pair** for that single row pair (`dist`)
 - **Average distance in KM T1 and T2 for tribe:** same as one in tribe level dataset above (`avg_dist` & `mid_dist`) which means the estimated average distance this tribe migrated
 - **Average Locations for tribe:** average centroid. same as one in tribe level dataset `amenity_tribes_level.csv` above
 - **Average Locations for tribe:** midpoint centroid. same as one in tribe level dataset `amenity_tribes_level.csv` above
 -  **Total number of locations at T1 versus T2 for tribe**: Count of unique counties at T1 and T2 (`n_unique_FIPS_t1` & `n_unique_FIPS_t2`)
 -  **Total Number of Possible Pairwise Migration Records for tribe:** total number of pairwise migration possibilities for a tribe. All possible routes from T1 to T2. (`n_record`)
