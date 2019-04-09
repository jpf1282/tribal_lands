rm(list = ls())

source("R/00-set-up.R")

target = "gas"
source("R/01-prep-data.R")
rmarkdown::render("R/02-analyze-gas-subset-main.R",
                  output_file = paste0("analysis-", target, "-subset-main.pdf"),
                  output_dir = pdf_path)
rmarkdown::render("R/03-analyze-gas-fullset.R",
                  output_file = paste0("analysis-", target, "-fullset.pdf"),
                  output_dir = pdf_path)
rmarkdown::render("R/04-analyze-gas-subset-tiny.R",
                  output_file = paste0("analysis-", target, "-subset-tiny.pdf"),
                  output_dir = pdf_path)


target = "risk"
source("R/01-prep-data.R")
rmarkdown::render("R/03-analyze-risk-fullset.R",
                  output_file = paste0("analysis-", target, "-fullset.pdf"),
                  output_dir = pdf_path)


target = "precipitation"
source("R/01-prep-data.R")
rmarkdown::render("R/03-analyze-precip-fullset.R",
                  output_file = paste0("analysis-", target, "-fullset.pdf"),
                  output_dir = pdf_path)


target = "amen_rank"
source("R/01-prep-data.R")
rmarkdown::render("R/03-analyze-amenrank-fullset.R",
                  output_file = paste0("analysis-", target, "-fullset.pdf"),
                  output_dir = pdf_path)
