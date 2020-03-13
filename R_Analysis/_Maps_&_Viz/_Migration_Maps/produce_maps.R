library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(rlang)
library(cowplot)
library(conflicted)
library(purrr)

#Resolve conflicts between packages
conflict_prefer("filter", "dplyr")

setwd("R_Analysis/_Maps_&_Viz/_Migration_Maps/") #don't need this right now
load('mdral.RData')

source("map_functions.R")


#########################################################################
#Function to produce maps for manuscript
ms_figure()

#Function to produce maps for appendix (6 per page; toggle legend at bottom of page)
app_figures(legend = F)
