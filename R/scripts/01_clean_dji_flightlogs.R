################################################################
# Author: Christian Eckert
# Contact: c.eckert.10@student.scu.edu.au
# Project: One Tree Island meteorology analysis
# Description: Process InterMet iMet-XQ2 uav atmospheric sensor data (flightlogs)
# Last Updated: 26-05-2025
################################################################


# Clear environment ------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(tidyverse)
library(data.table)
library(stringr)
library(here)         # ensures portability across systems

# Source cleaning function -----------------------------------------------------
source(here("R", "scripts", "00_functions_clean.R"))  

# Set file paths ---------------------------------------------------------------
input_dir <- here("data", "raw", "dji_flightrecords")
output_dir <- here("data", "processed", "clean_dji_flightrecords")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# List all DJI CSV files -------------------------------------------------------
drone_files <- list.files(path = input_dir, pattern = "\\.csv$", full.names = TRUE)

# Process and write cleaned files ---------------------------------------------
invisible(sapply(drone_files, function(file) {
  message("Cleaning: ", basename(file))
  raw_data <- fread(file, stringsAsFactors = TRUE) %>%
    slice(-1)
  clean_drone(raw_data, output_dir)
}))

# End script -------------------------------------------------------