- changes to make to data for analysis:
    - reverse direction of the change score. Take t2-t1, so that a negative score means loss of land quality
    - insert into 'merged_data_tribe' the average change score at tribe level. Simple calculation b/c t1 and t2 averages are there. Just need to take different.


merged_data_record
merged_data_tribe


hist(merged_data_record$amenity_change_score)

hist(merged_data_tribe$avg_amen_rank_t1)
hist(merged_data_tribe$avg_amen_rank_t2)
merged_data_tribe$amenity_change_score <- merged_data_tribe$avg_amen_rank_t1 - merged_data_tribe$avg_amen_rank_t2 #create change score variable taken from tribe averages that isn't in dataset...
hist(merged_data_tribe$amenity_change_score) 

# Create master change score for all data (average of all change scores)
merged_data_tribe %>%
  group_by(time) %>%
  summarise(mean=mean(amenity_change_score), n=n(), na.rm = T)

