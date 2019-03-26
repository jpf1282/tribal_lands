plot_t1t2 = function(df_long, 
                     use_notch = T,
                     use_scale = "log10",
                     fig_ylab = "",
                     fig_cap = "NAs were dropped") {
        # df_long: long format version
        
        # boxplot: show distribution of target at t1 and t2 side by side
        f = mk_boxplot(df_long)
        f("time", "val", notched = use_notch) %>% 
                add_labs(ylab = fig_ylab, 
                         caption = fig_cap) %>% 
                scale_axis(axis = "y", scale = use_scale) %>% 
                print()
}

