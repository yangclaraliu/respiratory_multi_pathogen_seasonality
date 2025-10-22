# Load required libraries
library(tidyverse)
library(cowplot)
library(ggplot2)
library(qs)

# Load the data
LL_flu <- readRDS("results/LL_flu_v3.rds")
LL_rsv <- readRDS("results/LL_rsv_v4.rds")

# Load other required data (assuming these exist from your main analysis)
# You may need to adjust these paths based on your actual data structure
if(file.exists("data/processed/flu/")) {
  data_flu <- list.files("data/processed/flu/", pattern = "*.csv", full.names = TRUE) %>%
    map(read_csv)
  names(data_flu) <- gsub(".*/(.*)-Cases.csv", "\\1", list.files("data/processed/flu/", pattern = "*-Cases.csv"))
}

if(file.exists("data/processed/rsv/")) {
  data_rsv <- list.files("data/processed/rsv/", pattern = "*.csv", full.names = TRUE) %>%
    map(read_csv)
  names(data_rsv) <- gsub(".*/(.*)-Cases.csv", "\\1", list.files("data/processed/rsv/", pattern = "*-Cases.csv"))
}

# Define city order (you may need to adjust this based on your actual city names)
city_ordered <- c("Beijing", "Lanzhou", "Xian", "Suzhou", "Wuhan", "Wenzhou", "Guangzhou", "Yunfu")

# Source the analysis script
source("code/2_visualisation_fig3_component_comparable.R")
