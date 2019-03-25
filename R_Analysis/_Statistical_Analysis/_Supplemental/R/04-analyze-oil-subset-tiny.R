#' ---
#' title: Analysis of `r target`
#' author: ""
#' date: "`r Sys.Date()`"
#' output: pdf_document
#' ---

#+ include = FALSE
knitr::opts_chunk$set(comment = "", tidy = F, echo = F, warning = F, 
                      message = F, fig.width = 8, fig.height = 6)

options(scipen = 999)

#+ results = "asis"

#' # Data Prep
#' 
#' 1. Dropped records with missing `r target` values at t1 and t2.
#' Call the resulting data set `df`.
#' 2. Separated `df` into two subsets:
#'    - `df_tiny`: `r target` < 1000 at t1 or t2.
#'    - `df_main`: `r target` >= 1000 at t1 and t2
#' 3. Created long-format version:
#'    - `df_long`: long-format version of the full set `df`
#'    - `df_tiny_long`: long-format version of the subset `df_tiny`
#'    - `df_main_long`: long-format version of the subset `df_main`
#' 
#' # Analyze the full set `df_tiny`
 
tabulate_stats(df_tiny_long) %>% 
        knitr::kable(caption = paste("Sample Summary Statistics of", target))

#' ## Q1. Is there a difference between t1 and t2?
#'
#' ### Descriptive Analysis
#' 
plot_change(df_tiny, fig_tit = target)


plot_t1t2(df_tiny_long, use_notch = F, use_scale = "log1p", fig_ylab = target)

#' ### Statistical Analysis
#' 
run_aov(df_tiny_long)
run_lme(df_tiny_long)

#' # Q2. How are t1 and t2 related?

plot_t1_vs_t2(df_tiny, use_scale = "log1p", fig_tit = target)