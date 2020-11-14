#This script compares the tri from mapzen DEM and Tom's augmented ruggedness

library(pacman)
p_load(tidyverse,sf,USAboundaries,janitor)

jb_topo <- read_csv("R_Analysis/topo_us.csv")
tom_topo <- read_csv("R_Analysis/vrml.csv") %>%
  clean_names() %>%
  mutate(geoid=str_pad(geoid,5,"left","0"))

analysis_ds <- inner_join(jb_topo,tom_topo)

to_plot <- analysis_ds %>%
  select(tri_mean,vrm,vrml) %>%
  mutate_all(~as.numeric(scale(.))) 

to_plot %>%
  GGally::ggpairs() 

to_plot %>%
  skimr::skim()
