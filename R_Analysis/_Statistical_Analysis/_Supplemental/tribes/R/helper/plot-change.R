plot_change = function(dat, fig_tit = "", fig_cap = "NAs were dropped") {
        # dat: data frame with change score, not the long format
        
        # density plot: show distribution of delta (t2-t1)
        f = mk_densityplot(dat)
        f("change_score", cut_tail = 0, font_size = 9) %>% 
                add_labs(title = fig_tit, caption = fig_cap) %>% 
                print()
        
        # qqnormal plot (detrended): check normality of delta (t2-t1)
        f = test_normality(dat)
        f("change_score", font_size = 9) %>% 
                add_labs(caption = fig_cap) %>% 
                print()
}


plot_change_discrete = function(dat, fig_tit = "", fig_cap = "NAs were dropped") {
        # dat: data frame with change score, not the long format
        
        dat = dat %>% mutate(change_score = factor(change_score))
        
        f = mk_barplot_freq(dat)
        f("change_score") %>% 
                add_labs(title = fig_tit, caption = fig_cap) %>% 
                print()
}
        
