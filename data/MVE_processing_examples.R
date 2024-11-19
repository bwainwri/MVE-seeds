# KM Hall
# 20231017
#
# Examples of additional options for processing of the data files after running
# the MVE_Process_Site_by_Year.R script for the site(s) and year(s) of interest.
# My example is for MVE Blue for 2023 and 2022.



library(tidyverse)
library(lubridate)
setwd("/Users/brookewainwright/Documents/Mean-Variance-seeds")

# CHANGE TO PATH WHERE THE PROCESSED FILES ARE LOCATED
path_to_files <- "/Users/brookewainwright/Documents/Mean-Variance-seeds/mve_data_processing/output/"


read_in_processed_data <- function(file_name) {
  # Function to help read in processed data files
  # The file name needs to be in quotes
  read_csv(paste0(path_to_files, file_name))
}

# load files on interest - YOU'LL NEED TO CHANGE THESE TO SUIT YOUR PURPOSE
blue_23 <- read_in_processed_data("MVE_PlainsGrassland_SoilMoistureTemperature_2023.csv")
blue_22 <- read_in_processed_data("MVE_PlainsGrassland_SoilMoistureTemperature_2022.csv")
blue_21 <- read_in_processed_data("MVE_PlainsGrassland_SoilMoistureTemperature_2021.csv")
blue_20 <- read_in_processed_data("MVE_PlainsGrassland_SoilMoistureTemperature_2020.csv")
blue_19 <- read_in_processed_data("MVE_PlainsGrassland_SoilMoistureTemperature_2019.csv")
head(blue_23)

# Get observation dates
setwd("/Users/brookewainwright/Documents/Mean-Variance-seeds")
blue19<-read.csv("Blue_2019_20221012.csv")
str(blue19)
unique(blue19$Obs_Date)

# Select dates that are 28, 14, or 7 days prior
# You could just do 7 day period, since that what was best before.

# Do daily means and then merge all 5
sm_daily_summary_19 <- blue_19 %>% 
  group_by(TIMESTAMP, plot, sensor, depth) %>% 
  summarize(sm_mean = round(mean(value, na.rm = TRUE), 3))
head(sm_daily_summary_19)

## Soil Moisture meanvarblue2019
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2019bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_VWC means_2019bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.VWC.7.12:mean.VWC.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
sm_for_join_19 <- blue_19 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_20 <- blue_20 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_21 <- blue_21 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_22 <- blue_22 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join<-rbind(sm_for_join_19, sm_for_join_20, sm_for_join_21, sm_for_join_22)
str(sm_for_join)

cns_7_sm <- census_7 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_14_sm <- census_14 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_28_sm <- census_28 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

sm_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- sm_summary_all_plots %>% 
  mutate(mean_soil_moisture = round(mean_soil_moisture, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_soil_moisture")

ggplot(template_all_plots, aes(x = census, y = mean_VWC_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Soil Moisture",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Soil Moisture")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_soil_moisture_2019.csv")

## Temperature meanvarblue2019
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2019bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_T means_2019bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.T.7.12:mean.T.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
t_for_join_19 <- blue_19 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_20 <- blue_20 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_21 <- blue_21 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_22 <- blue_22 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join<-rbind(t_for_join_19, t_for_join_20, t_for_join_21, t_for_join_22)
str(t_for_join)

cns_7_t <- census_7 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_14_t <- census_14 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_28_t <- census_28 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

t_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- t_summary_all_plots %>% 
  mutate(mean_temp = round(mean_temp, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_temp")

ggplot(template_all_plots, aes(x = census, y = mean_T_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Temperature",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Temperature")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_temperature_2019.csv")


## Soil Moisture meanvarblue2020
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2020bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_VWC means_2020bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.VWC.7.12:mean.VWC.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
sm_for_join_20 <- blue_20 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_21 <- blue_21 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_22 <- blue_22 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_23 <- blue_23 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)


sm_for_join<-rbind(sm_for_join_20, sm_for_join_21, sm_for_join_22, sm_for_join_23)
str(sm_for_join)

cns_7_sm <- census_7 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_14_sm <- census_14 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_28_sm <- census_28 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

sm_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- sm_summary_all_plots %>% 
  mutate(mean_soil_moisture = round(mean_soil_moisture, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_soil_moisture")

ggplot(template_all_plots, aes(x = census, y = mean_VWC_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Soil Moisture",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Soil Moisture")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_soil_moisture_2020.csv")

## Temperature meanvarblue2020
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2020bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_T means_2020bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.T.7.12:mean.T.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
t_for_join_20 <- blue_20 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_21 <- blue_21 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_22 <- blue_22 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_23 <- blue_23 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join<-rbind(t_for_join_20, t_for_join_21, t_for_join_22, t_for_join_23)
str(t_for_join)

cns_7_t <- census_7 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_14_t <- census_14 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_28_t <- census_28 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

t_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- t_summary_all_plots %>% 
  mutate(mean_temp = round(mean_temp, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_temp")

ggplot(template_all_plots, aes(x = census, y = mean_T_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Temperature",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Temperature")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_temperature_2020.csv")


## Soil Moisture meanvarblue2021
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2021bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_VWC means_2021bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.VWC.7.12:mean.VWC.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
sm_for_join_21 <- blue_21 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_22 <- blue_22 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_23 <- blue_23 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)


sm_for_join<-rbind(sm_for_join_21, sm_for_join_22, sm_for_join_23)
str(sm_for_join)

cns_7_sm <- census_7 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_14_sm <- census_14 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_28_sm <- census_28 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

sm_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- sm_summary_all_plots %>% 
  mutate(mean_soil_moisture = round(mean_soil_moisture, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_soil_moisture")

ggplot(template_all_plots, aes(x = census, y = mean_VWC_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Soil Moisture",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Soil Moisture")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_soil_moisture_2021.csv")

## Temperature meanvarblue2021
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2021bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_T means_2021bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.T.7.12:mean.T.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
t_for_join_21 <- blue_21 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_22 <- blue_22 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_23 <- blue_23 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join<-rbind(t_for_join_21, t_for_join_22, t_for_join_23)
str(t_for_join)

cns_7_t <- census_7 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_14_t <- census_14 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_28_t <- census_28 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

t_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- t_summary_all_plots %>% 
  mutate(mean_temp = round(mean_temp, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_temp")

ggplot(template_all_plots, aes(x = census, y = mean_T_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Temperature",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Temperature")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_temperature_2021.csv")


## Soil Moisture meanvarblue2022
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2022bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_VWC means_2022bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.VWC.7.12:mean.VWC.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
sm_for_join_22 <- blue_22 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

sm_for_join_23 <- blue_23 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)


sm_for_join<-rbind(sm_for_join_22, sm_for_join_23)
str(sm_for_join)

cns_7_sm <- census_7 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_14_sm <- census_14 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_28_sm <- census_28 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

sm_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- sm_summary_all_plots %>% 
  mutate(mean_soil_moisture = round(mean_soil_moisture, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_soil_moisture")

ggplot(template_all_plots, aes(x = census, y = mean_VWC_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Soil Moisture",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Soil Moisture")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_soil_moisture_2022.csv")

## Temperature meanvarblue2022
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2022bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_T means_2022bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.T.7.12:mean.T.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
t_for_join_22 <- blue_22 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join_23 <- blue_23 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

t_for_join<-rbind(t_for_join_22, t_for_join_23)
str(t_for_join)

cns_7_t <- census_7 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_14_t <- census_14 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_28_t <- census_28 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

t_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- t_summary_all_plots %>% 
  mutate(mean_temp = round(mean_temp, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_temp")

ggplot(template_all_plots, aes(x = census, y = mean_T_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Temperature",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Temperature")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_temperature_2022.csv")

## Soil Moisture meanvarblue2023
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2023bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_VWC means_2023bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.VWC.7.12:mean.VWC.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
sm_for_join <- blue_23 %>% 
  filter(sensor == "VWC") %>% 
  select(TIMESTAMP, plot, depth, value)

cns_7_sm <- census_7 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_14_sm <- census_14 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

cns_28_sm <- census_28 %>% 
  full_join(sm_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_sm %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_soil_moisture = mean(value, na.rm = TRUE))

sm_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- sm_summary_all_plots %>% 
  mutate(mean_soil_moisture = round(mean_soil_moisture, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_soil_moisture")

ggplot(template_all_plots, aes(x = census, y = mean_VWC_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Soil Moisture",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Soil Moisture")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_soil_moisture_2023.csv")

## Temperature meanvarblue2022
# Read in what I want the data to look like
# census file
census <- readxl::read_xlsx("census_dates_2023bluecohort.xlsx")

# template file
template <- readxl::read_xlsx("Empty Matrix_T means_2023bluecohort.xlsx")

# pivot template to long and delete the column containing the NAs for now
template_pv <- template %>% 
  pivot_longer(mean.T.7.12:mean.T.28.37, names_to = "full_var_name", values_to = "values") %>% 
  select(-values) %>% 
  separate(full_var_name, into = c("junk1", "junk2", "time_span", "depth"), remove = FALSE) %>% 
  select(-c(junk1, junk2))

# join census and template info
census_template <- census %>% 
  left_join(template_pv)
str(census_template)

# convert time_span and depth to numeric and convert date to date format
census_template <- census_template %>% 
  mutate(date = ymd(date),
         time_span = as.numeric(time_span),
         depth     = as.numeric(depth),
         end_date = date,
         start_date = date - time_span) %>% 
  select(-date)
str(census_template)

# splitting by look back periods into separate data sets

census_7 <- census_template %>% 
  filter(time_span == 7)

census_14 <- census_template %>% 
  filter(time_span == 14)

census_28 <- census_template %>% 
  filter(time_span == 28)

# joining census and sm data ----

# first only retain sm vars of interest
t_for_join <- blue_23 %>% 
  filter(sensor == "T") %>% 
  select(TIMESTAMP, plot, depth, value)

cns_7_t <- census_7 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_7_summary <- cns_7_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_14_t <- census_14 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_14_summary <- cns_14_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

cns_28_t <- census_28 %>% 
  full_join(t_for_join, by = "depth") %>% 
  filter(TIMESTAMP < end_date & TIMESTAMP >= start_date)
# TODO: Brooke!!!!! Make sure you want to include the end date and exclude the start date
#    You may have to adjust >=, <=, >, < to accommodate your specific window of interest

cns_28_summary <- cns_28_t %>% 
  select(-c(time_span, depth, start_date, TIMESTAMP)) %>% 
  group_by(census, full_var_name, plot) %>% 
  summarize(mean_temp = mean(value, na.rm = TRUE))

t_summary_all_plots <- rbind(cns_7_summary, cns_14_summary, cns_28_summary) %>% 
  mutate(full_var_name = str_replace_all(full_var_name, "[.]", "_"))

nrow(sm_summary_all_plots) == sum(nrow(cns_7_summary), nrow(cns_14_summary), nrow(cns_28_summary))
# evaluates to TRUE

# converting back to XLSX template - for ALL plots!!!! - this is what is needed----
# rounding values to 3 after decimal, and giving variables better names (_ instead of .)
template_all_plots <- t_summary_all_plots %>% 
  mutate(mean_temp = round(mean_temp, 3)) %>% 
  pivot_wider(names_from = "full_var_name", values_from = "mean_temp")

ggplot(template_all_plots, aes(x = census, y = mean_T_7_12)) + 
  geom_line() +
  facet_wrap(~ plot) +
  labs(title = "Mean Temperature",
       subtitle = "7 day look back at 12 cm depth") +
  xlab("Census Period") +
  ylab("Mean Temperature")


# write file ----
write_csv(template_all_plots, "mve_blue_mean_temperature_2023.csv")


# combine multiple years of data for a site - you can use more than two years
#blue_combined <- rbind(blue_19, blue_20, blue_21, blue_22, blue_23) |> arrange(TIMESTAMP, plot, depth, sensor_id)

#head(blue_combined)

# example: graph data for a particular plot and year -
# in this example only looking at 2023 data for plot P1
blue_combined |> 
  filter(year(TIMESTAMP) == 2023 & plot == "P1") |> 
  ggplot(aes(x = TIMESTAMP, y = value, color = sensor_id)) +
  geom_line(linewidth = .4) + 
  facet_wrap(~ sensor_id, scales = "free_y")

# example: graph data for a particular plot - 
# in this example looking at plot P1
blue_combined |> 
  filter(plot == "P1") |> 
  ggplot(aes(x = TIMESTAMP, y = value, color = sensor_id)) +
  geom_line(linewidth = .4) + 
  facet_wrap(~ sensor_id, scales = "free_y")


# example of pivoting the data back to original wide format (except the deleted 'RECORD' variable) - 
# the raw data was originally in a wide format, but there were several header rows in 
# the raw data files. 

## Left off here, think about what structure I want it in
#  to calculate 14 and 7-day averages and to merge with data

blue_combined_wide <- blue_combined |> 
  select(-c("new")) |> 
  pivot_wider(names_from = "sensor_id", values_from = "value")
head(blue_combined_wide)

## this is too big -- I should really reduce them first to those
#  XX-day averages, then combine

# example of writing the combined wide blue data to the output folder
write_csv(blue_combined_wide, paste0(path_to_files, "blue_combined_wide.csv"))









