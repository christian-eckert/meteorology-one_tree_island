################################################################
# Author: Christian Eckert
# Contact: c.eckert.10@student.scu.edu.au
# Project: One Tree Island meteorology analysis
# Description: Process InterMet iMet-XQ2 uav atmospheric sensor data (merge data)
# Last Updated: 26-05-2025
################################################################

# Clear environment
rm(list = ls())

# Load necessary packages
library(tidyverse)
library(data.table)
library(here)

# Source cleaning function -----------------------------------------------------
source(here("R", "scripts", "00_functions_clean.R"))  


# Load wind data
file_wind <- read_csv(here("data/raw/wind/wind.csv"), show_col_types = FALSE)

# Define output paths
wind_merge_filepath_write <- here("data/processed/merged_imet_wind/")
wind_merge_interpolated_filepath_write <- here("data/processed/interpolated_imet_wind/")

# Ensure output directories exist
dir.create(wind_merge_filepath_write, recursive = TRUE, showWarnings = FALSE)
dir.create(wind_merge_interpolated_filepath_write, recursive = TRUE, showWarnings = FALSE)

# List iMet files
iMet_listcsv <- list.files(here("data/processed/clean_imet/ascent/"), 
                           pattern = "\\.csv$", full.names = TRUE)

# Process all iMet5 ascent files
for (i in seq_along(iMet_listcsv)) {
  file_iMet <- read_csv(iMet_listcsv[i], show_col_types = FALSE)
  
  
  merge_wind(
    file_iMet = file_iMet,
    file_wind = file_wind,
    wind_merge_filepath_write = wind_merge_filepath_write,
    wind_merge_interpolated_filepath_write = wind_merge_interpolated_filepath_write
  )
  
  message("Processed: ", basename(iMet_listcsv[i]))
}

# End script -----------------------------------------------------
