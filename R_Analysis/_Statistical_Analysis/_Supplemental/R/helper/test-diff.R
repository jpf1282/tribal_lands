run_aov = function(df_long) {
        # df_long: long format version
        
        # 1-way Repeated Measure ANOVA
        aov_res = aov(val ~ time + Error(tribe / time), data = df_long)
        cat("1-way Repeated Measure ANOVA Output:\n")
        print(summary(aov_res))
}

run_lme = function(df_long) {
        # df_long: long format version
        
        # Linear Mixed Model 
        lme_res = nlme::lme(val ~ time, data = df_long, random = ~1|tribe)
        # summary(lme_res)
        cat("Linear Mixed Model Output:\n")
        print(anova(lme_res))
}