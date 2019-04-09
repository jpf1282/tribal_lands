# --- install packages from cran --- #

pkgs_installed = installed.packages()

cran_pkgs = c("devtools", "tidyverse", "broom", "knitr", "kableExtra", "e1071")
for (pkg in cran_pkgs) {
        if (!pkg %in% pkgs_installed) {
                cat(paste(pkg, "missing, will attempt to install\n"))
                install.packages(pkg, dependencies = T)
        } else cat(paste(pkg, "installed OK\n"))
}

github_pkgs = c("ezplot")
for (pkg in github_pkgs) {
        if (!pkg %in% installed.packages()) {
                cat(paste(pkg, "missing, will attempt to install\n"))
                if (pkg == "ezplot")
                        devtools::install_github(paste0("gmlang/", pkg))
        } else cat(paste(pkg, "installed OK\n"))
}

# --- load packages --- #

library(readr)
library(dplyr)
library(tidyr)
library(e1071)
library(ezplot)

# --- set up --- #

# options(scipen = 999)

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