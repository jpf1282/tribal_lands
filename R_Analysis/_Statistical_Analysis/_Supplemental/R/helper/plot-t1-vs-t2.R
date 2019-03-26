plot_t1_vs_t2 = function(dat, 
                         use_scale = "log10",
                         fig_tit = "",
                         fig_cap = "Each dot is a tribe") {
        
        # dat: a data frame, not the long format
        
        f = mk_scatterplot(
                dat %>% group_by(tribe) %>% 
                        summarise(avg_t1 = mean(t1),
                                  avg_t2 = mean(t2),
                                  med_t1 = median(t1),
                                  med_t2 = median(t2)) %>% ungroup()
                )

        f("avg_t1", "avg_t2") %>% 
                scale_axis(axis = "y", scale = use_scale) %>%
                scale_axis(axis = "x", scale = use_scale) %>% 
                add_lm_line() %>% 
                add_labs(xlab = "Mean value at t1", 
                         ylab = "Mean value at t2",
                         title = fig_tit,
                         caption = fig_cap) %>% 
                print()
        
        f("med_t1", "med_t2") %>% 
                scale_axis(axis = "y", scale = use_scale) %>%
                scale_axis(axis = "x", scale = use_scale) %>% 
                add_lm_line() %>% 
                add_labs(xlab = "Median value at t1", 
                         ylab = "Median value at t2",
                         title = fig_tit,
                         caption = fig_cap) %>% 
                print()
}


