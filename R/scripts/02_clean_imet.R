################################################################
# Author: Christian Eckert
# Contact: c.eckert.10@student.scu.edu.au
# Project: One Tree Island meteorology analysis
# Description: Process InterMet iMet-XQ2 uav atmospheric sensor data (imet)
# Last Updated: 26-05-2025
################################################################

# Clear all ---------------------------------------------------------------
rm(list = ls())

# Load necessary packages -------------------------------------------------
library(tidyverse)
library(data.table)
library(here)

# Source cleaning function ------------------------------------------------
source(here("R", "scripts", "00_functions_clean.R"))  

# Load and combine DJI + wind data ----------------------------------------
drone_flightrecord <- here("data", "processed", "clean_dji_flightrecords")
drone_data <- read_and_combine_csv(drone_flightrecord)


# Process iMet data ------------------------------------------
imet_files <- list.files(
  path = here("data", "raw", "imet"),
  pattern = "*.csv",
  full.names = TRUE
)

imet_write_ascent <- here("data", "processed", "clean_imet", "ascent")
imet_write_descent <- here("data", "processed", "clean_imet", "descent")


for (file in imet_files) {
  message("Processing iMet: ", basename(file))
  file_imet <- read.csv(file) %>% slice(-(1:3))
  clean_imet(file_imet, drone_data, imet_write_ascent, imet_write_descent)
}


