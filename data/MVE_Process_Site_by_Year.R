# KM Hall
# 20231017
#
# Revised MVE data processing program. The program wraps the data processing in a function
# that can be called from the command line. The arguments to the function are 'site' and
# 'year' (year of data to process). More info about the function call is provided below.
#
# This program should work on most computers and does not require running anything at the 
# command line.
#
# The function converts the data to a long format, and pulls out the various pieces
# of information that are embedded in the 'sensor_id' variable. This is helpful for
# graphing and other analyses.
#
#
# process_mve takes two arguments:
# site:
#  - blue
#  - black
#  - creosote
#  - pj
#  - jsav
#
# year:
#  numeric 4-digit year (e.g. - 2020)
#  
# The 'site' argument needs to be in quotes (e.g. - "blue"). The 'year' argument is not
# in quotes (e.g. 2023).




library(tidyverse)
library(lubridate)

# path to where the load_MVE_data_into_R.R file is located - CHANGE THIS TO THE PATH ON YOUR COMPUTER
source("~/Documents/SEV/Requests/mve_data_processing/load_MVE_data_into_R.R")





process_mve <- function(site, year_to_process) {
  # process_mve function:
  # 
  # Takes raw .dat MVE files as input and produces a long version of the data the data
  # as output.
  #
  # Function arguments: 
  # site: "blue", "black", "creosote", "pj", "jsav" are the choices for site, entered into
  #        the function with quotes
  # year: 4-digit year (YYYY) entered into the function without quotes
  
  
  
  # subsets data to be >= to year requested here
  filter_to_year <- year_to_process
  
  
  # folder where final data will be written - CHANGE THIS TO THE PATH ON YOUR COMPUTER WHERE YOU 
  # WANT TO WRITE OUT THE PROCESSED FILES
  folder_out <- "~/Documents/SEV/Requests/mve_data_processing/output/"
  

  # name of final output file for year of data being processed
  sub_file_name <- if (site == 'blue') {
    paste0("MVE_PlainsGrassland_SoilMoistureTemperature_", filter_to_year, ".csv")
  } else if (site == "black") {
    paste0("MVE_DesertGrassland_SoilMoistureTemperature_", filter_to_year, ".csv")
  } else if (site == "creosote") {
    paste0("MVE_Creosote_SoilMoistureTemperature_", filter_to_year, ".csv")
  } else if (site == "pj") {
    paste0("MVE_PJ_SoilMoistureTemperature_", filter_to_year, ".csv")
  } else if (site == "jsav") {
    paste0("MVE_JSav_SoilMoistureTemperature_", filter_to_year, ".csv")
  } 
  
  # load MVE data ------------------------------------------------------------
  
  
  file_to_load <- if (site == "blue") {
    "MVE_Blue.dat"
  } else if (site == "black") {
    "MVE_Black.dat"
  } else if (site == "creosote") {
    "MVE_Creosote.dat"
  } else if (site == "pj") {
    "MVE_PJ_Table1_52423_MVC_2023_05_24_10_49_50.dat"
  } else if (site == "jsav") {
    "MVE_JSAV_Table1_52423_MVC_2023_05_24_12_46_59.dat"
  } else {
    NULL
  }
  
  mve <- read_mve_in(file_to_load) |> 
    select(-RECORD)
  
  
  # Note: For Blue, data prior to 2022-10-04 08:00:00 needs to be loaded from a file
  # because the data loggers were reset to capture data from some new sensors that
  # were installed. 
  
  # loading file for old MVE Blue data - 
  mve_blue_old <- if (site == "blue") {
    read_mve_in("MVE_Blue.dat.backup") |> 
      mutate(VWC_P2_12_NEW = as.numeric(NA),               # Need to add these new variables as NAs to old data in order to combine with newer data
             VWC_P2_22_NEW = as.numeric(NA),
             VWC_P2_37_NEW = as.numeric(NA)) |> 
      select(-RECORD)
  } else {
    NULL
  }
  
  
  mve_sub <- if (site == "blue") {
    rbind(mve, mve_blue_old) |> 
      arrange(TIMESTAMP) |> 
      unique() |> 
      filter(year(TIMESTAMP) == filter_to_year)
  } else {
    mve |> 
      arrange(TIMESTAMP) |> 
      unique() |> 
      filter(year(TIMESTAMP) == filter_to_year)
  }
  
  
  
  
  mve_sub_long <- if (site == "blue") {
    mve_sub |> 
      pivot_longer(-TIMESTAMP, names_to = "sensor_id") |> 
      separate(sensor_id, into = c("sensor", "plot", "depth", "new"), sep = "_", remove = FALSE) 
  } else if (site == "black") {
    mve_sub |> 
      pivot_longer(-TIMESTAMP, names_to = "sensor_id") |> 
      separate(sensor_id, into = c("plot", "depth", "sensor", "avg"), sep = "_", remove = FALSE)
  } else if (site == "creosote") {
    mve_sub |> 
      pivot_longer(-TIMESTAMP, names_to = "sensor_id") |> 
      separate(sensor_id, into = c("sensor", "piece1", "piece2", "piece3", "avg"), sep = "_", remove = FALSE) |> 
      mutate(plot1split = ifelse(piece1 %in% c(2, 3), NA, piece1),
             plot2split = ifelse(piece2 %in% c(12, 22, 37), NA, piece2),
             depth1split = ifelse(piece2 %in% c(12, 22, 37), piece2, NA),
             depth2split = ifelse(piece3 %in% c(12, 22, 37), piece3, NA),
             plot_extra = ifelse(piece1 %in% c(2, 3), piece1, NA),
             plot = ifelse((!is.na(plot1split) & is.na(plot2split)), plot1split, plot2split),
             depth = ifelse((!is.na(depth1split) & is.na(depth2split)), depth1split, depth2split)) |> 
      select(-c(avg, piece1, piece2, piece3, plot1split, plot2split, depth1split, depth2split))
  } else if (site == "pj") {
    mve_sub |> 
      pivot_longer(-TIMESTAMP, names_to = "sensor_id") |> 
      separate(sensor_id, into = c("sensor", "piece1", "piece2", "piece3", "piece4"), sep = "_", remove = FALSE) |> 
      mutate(plot1split = ifelse(piece1 %in% c(2, 3), NA, piece1),
             plot2split = ifelse(piece2 %in% c(12, 22, 37), NA, piece2),
             depth1split = ifelse(piece2 %in% c(12, 22, 37), piece2, NA),
             depth2split = ifelse(piece3 %in% c(12, 22, 37), piece3, NA),
             plot_extra = ifelse(piece1 %in% c(2, 3), piece1, NA),
             plot = ifelse((!is.na(plot1split) & is.na(plot2split)), plot1split, plot2split),
             depth = ifelse((!is.na(depth1split) & is.na(depth2split)), depth1split, depth2split)) |> 
      select(-c(piece1, piece2, piece3, piece4, plot1split, plot2split, depth1split, depth2split))
  } else if (site == "jsav") {
    mve_sub |> 
      pivot_longer(-TIMESTAMP, names_to = "sensor_id") |> 
      separate(sensor_id, into = c("sensor", "piece1", "piece2", "piece3", "piece4"), sep = "_", remove = FALSE) |> 
      mutate(plot1split = ifelse(piece1 %in% c(2, 3), NA, piece1),
             plot2split = ifelse(piece2 %in% c(12, 22, 37), NA, piece2),
             depth1split = ifelse(piece2 %in% c(12, 22, 37), piece2, NA),
             depth2split = ifelse(piece3 %in% c(12, 22, 37), piece3, NA),
             plot_extra = ifelse(piece1 %in% c(2, 3), piece1, NA),
             plot = ifelse((!is.na(plot1split) & is.na(plot2split)), plot1split, plot2split),
             depth = ifelse((!is.na(depth1split) & is.na(depth2split)), depth1split, depth2split)) |> 
      select(-c(piece1, piece2, piece3, piece4, plot1split, plot2split, depth1split, depth2split))
  } else {
    NULL
  }
    
  write_csv(mve_sub_long, paste0(folder_out, sub_file_name))

  return(paste("Data processing complete for site", site, "for year", year_to_process))
  
  
}




# Note: you will get a warning message when running the function because of how the
# raw data is formatted and needs to be processed.

# running these functions writes processed data to the output folder - only need to do once
# for prior years, but need to run the current year each time you get new, updated raw data
process_mve("blue", 2023)
process_mve("blue", 2022)
process_mve("blue", 2021)
process_mve("blue", 2020)
process_mve("blue", 2019)
process_mve("blue", 2018)

process_mve("black", 2023)
process_mve("black", 2022)
process_mve("black", 2021)
process_mve("black", 2020)
process_mve("black", 2019)

process_mve("creosote", 2023)
process_mve("creosote", 2022)

process_mve("jsav", 2023)
process_mve("jsav", 2022)

process_mve("pj", 2023)
process_mve("pj", 2022)








