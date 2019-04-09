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
                                  med_t2 = median(t2)) %>% ungroup())

        f("avg_t1", "avg_t2", jitter = F) %>% 
                scale_axis(axis = "y", scale = use_scale) %>%
                scale_axis(axis = "x", scale = use_scale) %>% 
                add_lm_line() %>% 
                add_labs(xlab = "Mean value at t1", 
                         ylab = "Mean value at t2",
                         title = fig_tit,
                         caption = fig_cap) %>% 
                print()
        
        f("med_t1", "med_t2", jitter = F) %>% 
                scale_axis(axis = "y", scale = use_scale) %>%
                scale_axis(axis = "x", scale = use_scale) %>% 
                add_lm_line() %>% 
                add_labs(xlab = "Median value at t1", 
                         ylab = "Median value at t2",
                         title = fig_tit,
                         caption = fig_cap) %>% 
                print()
}



plot_t1_vs_t2_mode = function(dat, 
                              fig_tit = "",
                              fig_cap = "Each dot is a tribe") {
        
        # dat: a data frame, not the long format
        
        ha = dat %>% group_by(tribe) %>% 
                summarise(mode_t1 = mean(modes(t1)),
                          mode_t2 = mean(modes(t2))) %>% 
                ungroup()
        
        f = mk_scatterplot(ha)
        
        f("mode_t1", "mode_t2", jitter = F) %>% 
                add_lm_line(show = "tb") %>%
                add_labs(xlab = "Most frequent value at t1", 
                         ylab = "Most frequent value at t2",
                         title = fig_tit,
                         caption = fig_cap) %>% 
                print()

        # fit lm and extract est and se
        fit = lm(mode_t2 ~ mode_t1, data = ha)
        rsqrd = broom::glance(fit) %>% pull(r.squared)
        out = broom::tidy(fit)
        out %>% filter(term == "mode_t1") %>% select(estimate, std.error) %>% 
                mutate(rr = rsqrd, n = nrow(ha))
}
