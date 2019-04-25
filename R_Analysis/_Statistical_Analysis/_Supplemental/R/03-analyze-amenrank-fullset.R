#' ---
#' title: Analysis of `r target`
#' author: ""
#' date: "`r Sys.Date()`"
#' output: pdf_document
#' ---

#+ include = FALSE
knitr::opts_chunk$set(comment = "", tidy = F, echo = F, warning = F, 
                      message = F, fig.width = 8, fig.height = 6)

# options(scipen = 0) # force scientific option

#+ results = "asis"

#' # Data Prep
#' 
#' 1. Dropped records with missing `r target` values at t1 and t2.
#' Call the resulting data set `df`.
#' 2. Created long-format version:
#'    - `df_long`: long-format version of the full set `df`
#' 
#' # Analyze the full set `df`
 
# cap = paste("Sample Summary Statistics of", target,
#             "(A normal distribution has mean = median, skewness = 0 and kurtosis = 0")
# tabulate_stats(df_long) %>% knitr::kable(caption = cap, digits = 3)

#' ## Q1. Is there a difference between t1 and t2?
#'
#' ### Descriptive Analysis
#' 
#' First we look at the overall distribution of the change scores (t2 - t1) 
#' without breaking down by tribes.
#' Because the raw values at t1/t2 are rank scores, the change scores take on
#' a finite number of values, namely `r sort(unique(df$change_score))`. 
#' The following bar chart shows their frequency counts. Our first impression is
#' the chart is rather bell-curvish looking. We see that
#' about 40% of the observations have a change score of 0, and about 87% of 
#' the observations have a change score of -1, 0 or 1. There're slightly (7%) 
#' more observations with rank increased by 1 than decreased by 1. 
#' There're slightly (2%) more observations with rank increased by 2 than 
#' decreased by 2. 
#' There're only small number of observations with rank increased or decreased 
#' by 3 or more. 

plot_change_discrete(df, fig_tit = target)

#' Next we look at the distribution of the change scores within each tribe.
#' The following heatmaps show that within each tribe, the bulk of the change 
#' scores also centers around -1, 0 and 1. 

uniq_delta = sort(unique(df$change_score))
by_tribe = df %>% count(tribe)
ha = lapply(uniq_delta, function(x) 
        setNames(df %>% group_by(tribe) %>% 
                         summarise(pct = sum(change_score == x) / n()) %>% 
                         select(pct), x)) %>% bind_cols()
by_tribe = bind_cols(by_tribe, ha) %>% arrange(n) %>% 
        mutate(tribe = factor(tribe, levels = tribe))

# cap = paste("Distribution of", target, "change scores by tribe")
# by_tribe %>% knitr::kable(caption = cap, digits = 4)

by_tribe_long1 = by_tribe %>% filter(n >= 80) %>% 
        select(-n) %>% gather(delta, val, -tribe) %>% 
        mutate(delta = factor(delta, levels = -5:4))
by_tribe_long2 = by_tribe %>% filter(n < 80, n >= 10) %>% 
        select(-n) %>% gather(delta, val, -tribe) %>% 
        mutate(delta = factor(delta, levels = -5:4))
by_tribe_long3 = by_tribe %>% filter(n < 10) %>% 
        select(-n) %>% gather(delta, val, -tribe) %>% 
        mutate(delta = factor(delta, levels = -5:4))

plt = mk_heatmap(by_tribe_long1)
plt("delta", "tribe", fillby = "val", legend_title = "Percent") %>%
        add_labs(caption = "tribes with at least 80 records")

plt = mk_heatmap(by_tribe_long2)
plt("delta", "tribe", fillby = "val", legend_title = "Percent") %>%
        add_labs(caption = "tribes with 10 - 79 records")

plt = mk_heatmap(by_tribe_long3)
plt("delta", "tribe", fillby = "val", legend_title = "Percent") %>%
        add_labs(caption = "tribes with less than 10 records")

#' Finally, we group the sample records into three cohorts based on change score: 
#' "No Change", "Increased" and "Decreased". We then look at the distribution of
#' these cohorts in a bar chart. 
df = df %>% mutate(yflag = case_when(change_score == 0 ~ "No Change",
                                     change_score > 0 ~ "Increased",
                                     change_score < 0 ~ "Decreased"))
f = mk_barplot_freq(df)
f("yflag") %>% add_labs(xlab = "Rank change (t2 - t1) category")

pct_of_cohorts = df %>% pull(yflag) %>% table() %>% prop.table()

#' ### Statistical Analysis
#' 
#' The sample percentages of "No Change", "Increased" and "Decreased" are 
#' `r formattable::percent(pct_of_cohorts[["No Change"]], 2)`,
#' `r formattable::percent(pct_of_cohorts[["Increased"]], 2)`, and 
#' `r formattable::percent(pct_of_cohorts[["Decreased"]], 2)`.
#' 
#' Let's test if there is some difference among the proportions of 
#' "No Change", "Increased" or "Decreased" in the population. Our null 
#' hypothesis is that there is an equal distribution (i.e., the proportions of 
#' "No Change", "Increased" and "Decreased" are 1/3 each). 

# --- chi-squared test --- #

O = pct_of_cohorts * 100
E = 1 / length(O) * 100
chisq_stat = sum((O - E) ^ 2) / E
ddf = length(O) - 1
pval = pchisq(chisq_stat, ddf, lower.tail = F)

#' We choose 0.05 significance level and run Chi-squared test. We conclude we 
#' don't have enough evidence to
#' reject the null (Chi-squared test statistic = `r round(chisq_stat, 2)`, 
#' pvalue = `r round(pval, 2)`). In other words, it's perfectly likely that 
#' a population with equal proportions of "No Change", "Increased" and "Decreased"
#' can produce a sample set like we have where the sample proportions are different.

# --- non-parametric test via bootstrap --- #

set.seed(9283)
B = 1000
x = df %>% pull(change_score)
n = length(x)
resamples = matrix(sample(x, n*B, replace = T), B, n)
means = apply(resamples, 1, mean)
ci95 = quantile(means, c(0.025, 0.975)) %>% round(3)

#' Next we want to test if the population mean of the change scores is different
#' from zero. Our null hypothesis is that it is zero. We take 1000
#' bootstrapped samples and estimate the 95% confidence interval of the mean to
#' be `r paste0("(", paste(ci95, collapse = ", "), ")")`. Because the 95% 
#' confidence interval sits entirely above zero, we conclude the mean change 
#' score is significantly different from zero (slightly bigger than zero). 
#' 
#' # Q2. How are t1 and t2 related?
#' 
#' We first calculate the most frequent (mode) `r target` values of each tribe at 
#' t1 and t2. We use the mode instead of the mean or median because `r target` is 
#' a ranking. We then make a scatterplot of the t2 vs. t1 modes, which showed 
#' strong positive linear relationships:
#' 
#' - **strong**: the tighter the dots, the stronger the correlation.
#' - **positive**: upward slanted trend from bottom left corner to upper right corner.
#' Or y tends to increase as x increases. 
#' 
#' Finally, we use linear regression to quantify this relationship.

fit_res = plot_t1_vs_t2_mode(df, fig_tit = target)
n = fit_res %>% pull(n)
rr = fit_res %>% pull(rr)
slope = fit_res %>% pull(estimate)
se = fit_res %>% pull(std.error)
MOE = moe_lm_param(se, n)  # margin of error
ci95 = slope + c(-1, 1) * MOE
ci95_str = paste0("(", paste(round(ci95, 3), collapse = ", "), ")")
        
#' We get a r-squared value of `r round(rr, 3)`, which translates
#' to a correlation of `r round(sqrt(rr), 3)` (by taking the squared root). 
#' This big positive correlation indicates the most frequent values at t1 and t2 
#' tend to rise and fall together. 
#' The slope of the line is `r round(slope, 3)` with a 95% confidence interval
#' of `r ci95_str`. (Both the confidence interval and the tiny p-value 
#' indicate the slope is statistically significant and hence not zero.) 
#' This implies that for every unit increase in `r target` at t1, we can expect 
#' an `r round(slope, 3)`-unit jump give or take `r round(MOE, 3)` at t2.

