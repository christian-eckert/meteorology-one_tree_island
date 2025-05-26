#########################################################################
# FUNCTION CLEAN DRONE DATA ------------------------------------------ #
#######################################################################

clean_drone <- function(file_drone, drone_filepath_write) {
  
  library(tidyverse)
  library(lubridate)
  library(data.table)
  
  # Define the column selection and new column names
  col_select_dji <- c(
    "time(millisecond)", 
    "datetime(utc)", 
    "latitude", 
    "longitude",
    "altitude_above_seaLevel(meters)", 
    "satellites", 
    "flycState", 
    "battery_percent")
  
  col_names_dji <- c(
    "milli", 
    "dji_datetime", 
    "dji_lat", 
    "dji_long", 
    "dji_alt",
    "dji_satcount", 
    "dji_flycstate", 
    "dji_battery")
  
 
  file_drone <- file_drone %>%
    select(col_select_dji) %>%
    rename_with(~ col_names_dji, col_select_dji) %>%
    filter_all(any_vars(!is.na(.) & !grepl("^\\s*$", .))) %>%
    group_by(dji_datetime) %>%
    summarize(
      dji_alt = mean(dji_alt),
      dji_lat = mean(dji_lat),
      dji_long = mean(dji_long),
      dji_battery = round(mean(dji_battery)),
      dji_satcount = round(mean(dji_satcount)),
      dji_flycstate = nth(dji_flycstate, 3)
    ) %>%
    mutate(
      #dji_datetime = parse_date_time(dji_datetime, orders = c("dmy HMS", "ymd HMS"), tz = "UTC"),
      datetime_aest = with_tz(dji_datetime, tzone = "Australia/Brisbane"),
      dji_epoch = as.numeric(dji_datetime),
      dji_day_local = format(datetime_aest, format = "%Y%m%d"),
      dji_take_off = case_when(
        between(hour(datetime_aest), 8, 10) ~ "09",
        between(hour(datetime_aest), 11, 13) ~ "12",
        between(hour(datetime_aest), 14, 16) ~ "15",
        TRUE ~ format(datetime_aest, format = "%H:%M:%S")
      )
    ) %>%
    select(-datetime_aest)
  
  # Find the index of the peak value
  peak_index <- which.max(file_drone$dji_alt)
  
  # Find the index of the take off value
  takeoff_index <- max(which(file_drone$dji_flycstate == "Assisted_Takeoff"))
  
  landing_index <- min(which(file_drone$dji_flycstate == "Confirm_Landing"))
  
  # Find the index of the first value crossing 10
  precheck_index <- min(which(file_drone$dji_alt > 10))
  
  # Find the index of the last value crossing 10
  postcheck_index <- max(which(file_drone$dji_alt > 10))
  
  # Find the index of the start mission value
  start_index <- peak_index - min(which(diff(sign(diff(ifelse(file_drone$dji_alt[(peak_index-1):precheck_index] < 30, file_drone$dji_alt[(peak_index-1):precheck_index], 0)))) == 2)) - 1
  
  end_index <- peak_index + min(which(diff(sign(diff(ifelse(file_drone$dji_alt[(peak_index+1):postcheck_index] < 30, file_drone$dji_alt[(peak_index+1):postcheck_index], 0)))) == 2)) + 1
  
  # Mark the rows
  file_drone$dji_status <- NA
  file_drone$dji_status[precheck_index] <- "precheck"
  file_drone$dji_status[postcheck_index] <- "postcheck"
  file_drone$dji_status[peak_index] <- "peak"
  file_drone$dji_status[start_index] <- "start"
  file_drone$dji_status[end_index] <- "end"
  file_drone$dji_status[takeoff_index] <- "takeoff"
  file_drone$dji_status[landing_index] <- "landing"
  
  # Mark rows between "start" and "peak" as "ascent"
  file_drone$dji_status[(start_index+1):(peak_index-1)] <- "ascent"
  
  # Mark rows between "peak" and "end" as "descent"
  file_drone$dji_status[(peak_index+1):(end_index-1)] <- "descent"
  
  # Drop rows with NA in the status column
  file_drone <- na.omit(file_drone) %>%
    select(-dji_flycstate)
  
  # Round the dji_alt column to two decimal places
  file_drone$dji_alt <- round(file_drone$dji_alt, 2)
  
  # Create filename
  drone_filename <- file.path(
    drone_filepath_write,
    paste0("clean_drone_", file_drone$dji_day_local[2], "_", file_drone$dji_take_off[2], ".csv")
  )
  
  # Write data
  write.csv(file_drone, drone_filename, row.names = FALSE)
  invisible(NULL) 
}


#############################################################################
# FUNCTION READ AND COMBINE DATA ------------------------------------------ #
############################################################################

read_and_combine_csv <- function(directory_path) {
  csv_files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)
  combined_data <- data.frame()
  
  for (file in csv_files) {
    file_data <- read_csv(file, show_col_types = FALSE)
    combined_data <- rbind(combined_data, file_data)
  }
  
  # Convert character columns to factors (if needed)
  char_cols <- unlist(sapply(combined_data, is.character))  # <- FIXED
  combined_data[, char_cols] <- lapply(combined_data[, char_cols], factor)
  
  return(combined_data)
}


##############################################################################
# FUNCTION CLEAN IMET ----------------------------------------------------- #
############################################################################

clean_imet <- function(file_iMet, drone_data, iMet_filepath_write_ascent, iMet_filepath_write_descent){
  
  library(tidyverse)
  library(lubridate)
  library(data.table)
  library(frost)
  
  # Define the column selection and new column names
  col_select_iMet <- c("XQ.iMet.XQ.Pressure",
                       "XQ.iMet.XQ.Air.Temperature",
                       "XQ.iMet.XQ.Humidity",
                       "XQ.iMet.XQ.Date",
                       "XQ.iMet.XQ.Time",
                       "XQ.iMet.XQ.Longitude",
                       "XQ.iMet.XQ.Latitude",
                       "XQ.iMet.XQ.Altitude")
  
  col_names_iMet <- c("pres",
                      "temp",
                      "hum",
                      "date",
                      "time",
                      "long",
                      "lat",
                      "alt")
  
  
  file_iMet <- file_iMet %>%
    select(all_of(col_select_iMet)) %>% 
    rename_with(~col_names_iMet, col_select_iMet) %>%
    slice(-(1:2)) %>%
    filter_all(any_vars(!is.na(.) & !grepl("^\\s*$", .))) %>%
    distinct(time, .keep_all = TRUE) %>%
    
    mutate(datetime_utc = parse_date_time(paste(date, time),
                                          orders = c("dmy HMS", "ymd HMS"),
                                          tz = "UTC"),
           datetime_aest = with_tz(datetime_utc, tzone = "Australia/Brisbane"),
           epoch = as.numeric(datetime_utc),
           day_local = format(datetime_aest, format = "%Y%m%d"),
           time_local = format(datetime_aest, format = "%H:%M:%S"),
           # Calculations of parameters Boundary Layer Height
           dewtemp = round(calcDewPoint(hum, temp, mode="C"),2),
           temp_k = temp + 273.5,
           svp = 6.1094 * exp(17.625 * temp / (243.04 + temp)),
           avp = (hum/100) * svp,
           mratio = (621.97 * avp / (pres - avp)),
           mratio2 = mratio *  0.001,
           pottemp = temp_k * (1000/pres)^(2/7),
           vpottemp = pottemp * (1 + 0.61 * mratio2),
           #
           take_off = case_when(
             between(hour(datetime_aest), 8, 10) ~ "09",
             between(hour(datetime_aest), 11, 13) ~ "12",
             between(hour(datetime_aest), 14, 16) ~ "15",
             TRUE ~ time_local
           )) %>%
    select(-date, -time, -time_local, -datetime_aest) # Remove the unnecessary columns
  
  # Define the new column selection 
  col_select_merge <- c("day_local",
                        "take_off",
                        "datetime_utc",
                        "epoch",
                        "dji_lat",
                        "dji_long",
                        "dji_alt",
                        "temp",
                        "hum",
                        "pres",
                        "dewtemp",
                        "temp_k",
                        "svp",
                        "avp",
                        "mratio",
                        "pottemp",
                        "vpottemp",
                        "dji_status")
  
  
  # Merge iMet data with drone data
  file_merge <- merge(file_iMet, drone_data, by.x = 'epoch', by.y = 'dji_epoch') %>% 
    select(all_of(col_select_merge)) 
  
  
  file_ascent <- file_merge %>%
    filter(dji_status %in% c("takeoff","precheck","start", "ascent", "peak"))
  
  file_descent <- file_merge %>% 
    filter(dji_status %in% c("peak", "descent", "end", "postcheck", "landing"))
  
  
  filename_ascent <- paste0("clean_ascent_", file_ascent$day_local[2], "_", file_ascent$take_off[2], ".csv")
  filename_descent <- paste0("clean_descent_", file_descent$day_local[2], "_", file_descent$take_off[2], ".csv")
  
  iMet_filename_ascent <- file.path(iMet_filepath_write_ascent, filename_ascent)
  iMet_filename_descent <- file.path(iMet_filepath_write_descent, filename_descent)
  
  
  # Write data
  write.csv(file_ascent, iMet_filename_ascent, row.names = FALSE)
  write.csv(file_descent, iMet_filename_descent, row.names = FALSE)
  
}


#############################################################################################
# MERGE WIND # MERGE WIND # MERGE WIND # MERGE WIND # MERGE WIND # MERGE WIND # MERGE WIND #
###########################################################################################
# FUNCTION MERGE WIND AND iMet DATA and interpolate  ------------------------------------
#########################################################################################

merge_wind <- function(file_iMet, file_wind, wind_merge_filepath_write, wind_merge_interpolated_filepath_write) {
  
  # clear all --------------------------------------------------------
  rm(list = ls())
  
  library(tidyverse)
  library(lubridate)
  library(data.table)
  library(fuzzyjoin)
  library(circular)
  library(zoo)
  library(cimir)
  
 
  unique_combinations <- file_iMet %>%
    distinct(day_local, take_off)
  
  # Create a subset of file_wind that matches the criteria
  file_wind <- file_wind %>%
    filter(day_local %in% unique_combinations$day_local, 
           take_off %in% unique_combinations$take_off) %>% 
    mutate(wind_alt = if_else(row_number() == 1, wind_alt - 0.5, wind_alt))
  
  
  file_iMet <- file_iMet %>%
    filter(dji_status %in% c("start", "ascent", "peak")) %>% 
    mutate(row_index = row_number())
  
  # Wind data --------------------------------------------------------------
  
  sub_file_wind <- file_wind %>%
    group_by(wind_alt) %>%
    mutate(closest_ws_index = which.min(abs(file_iMet$dji_alt - wind_alt))) %>%
    ungroup() 
  
  
  merge_file_iMet <- fuzzy_left_join(file_iMet, sub_file_wind, 
                         by = c("row_index" = "closest_ws_index"), 
                         match_fun = `==`)  %>% 
    select(-'day_local.y', -'take_off.y', -'closest_ws_index', -'row_index') %>% 
    rename(day_local = "day_local.x", take_off = "take_off.x")
  

  interpolated_file_iMet <- merge_file_iMet %>% 
    mutate(wspd = round(zoo::na.approx(wspd, na.rm = FALSE),1),
           u = round(zoo::na.approx(u, na.rm = FALSE),3),
           v = round(zoo::na.approx(v, na.rm = FALSE),3),
           dir = round(270 - atan2(v, u) * 180 / pi, 1),
           dir = ifelse(dir > 360, dir - 360, dir),
           dir = ifelse(dir < -360, dir + 360, dir),
           pottemp = round(pottemp,2),
           vpottemp = round(vpottemp,2),
           svp = round(svp,2),
           avp = round(avp,2),
           mratio = round(mratio,2)) %>% 
    select(-'rad', -'wd_char', -'wind_alt', -'time', -'date')
  
  # Create filename
  interpolated_filename <- file.path(
    wind_merge_interpolated_filepath_write,
    paste0("inter_iMet_wind_", interpolated_file_iMet$day_local[2], "_", interpolated_file_iMet$take_off[2], ".csv")
  )
  
  merge_filename <- file.path(
    wind_merge_filepath_write,
    paste0("orig_iMet_wind_", merge_file_iMet$day_local[2], "_", merge_file_iMet$take_off[2], ".csv")
  )
  
  # Write data
  write.csv(interpolated_file_iMet, interpolated_filename, row.names = FALSE)
  write.csv(merge_file_iMet, merge_filename, row.names = FALSE)
}
