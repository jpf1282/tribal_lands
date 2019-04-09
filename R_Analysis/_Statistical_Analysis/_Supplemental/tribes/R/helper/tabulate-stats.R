tabulate_stats = function(df_long) {
        # df_long: long format version of df
        df_long %>% group_by(time) %>% 
                summarise(n_tribes = length(unique(tribe)), 
                          n = n(), 
                          mean = mean(val), 
                          median = median(val),
                          std = sd(val),
                          skewness = skewness(val),
                          kurtosis = kurtosis(val) # result already -3
                          ) %>% 
                mutate(SEM = std / sqrt(n))
}
