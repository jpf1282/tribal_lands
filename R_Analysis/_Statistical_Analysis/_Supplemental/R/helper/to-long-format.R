to_long = function(df) {
        # Returns a data frame that's the long format of the given data frame.
        # Changes the variables "time" and "tribe" to factors.
        df %>% select(tribe, t1, t2) %>% 
                gather(time, val, -tribe) %>% 
                mutate(tribe = factor(tribe),
                       time = factor(time))
}
