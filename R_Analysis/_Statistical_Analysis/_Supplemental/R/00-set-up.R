library(readr)
library(dplyr)
library(tidyr)
library(ezplot) # install via devtools::install_github("gmlang/ezplot")

# set options
options(scipen = 999)

# set paths
data_path = "data"
out_path = "output"
pdf_path  = file.path(out_path, "pdf")
# png_path  = file.path(out_path, "png")
csv_path  = file.path(out_path, "csv")
dir.create(pdf_path, showWarnings = F, recursive = T)
# dir.create(png_path, showWarnings = F, recursive = T)
dir.create(csv_path, showWarnings = F, recursive = T)

# load helper functions
helper_path = "R/helper"
for (fname in list.files(helper_path)) source(file.path(helper_path, fname))