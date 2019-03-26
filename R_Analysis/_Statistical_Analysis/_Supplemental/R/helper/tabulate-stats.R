tabulate_stats = function(df_long) {
        # df_long: long format version of df
        df_long %>% group_by(time) %>% 
                summarise(n_tribes = length(unique(tribe)), n = n(), 
                          mean = mean(val), 
                          SEM = sd(val) / sqrt(n())
                          ) 
}
