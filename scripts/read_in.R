# libraries
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(readr)
library(suncalc)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(vroom)

source("https://raw.githubusercontent.com/bjscannell/lab_code/master/load_vue_csvs.R")
  #I (bvc) am unable to access this file, not sure if it's necessary?


# Code for Acoustics ------------------------------------------------------


detections <- read_csv("DATA/acoustic/shark_detections_2022-2024.csv") %>% clean_names() %>% as.data.frame()
# above csv is filtered through glatos


shark_atag <- read_csv("tag/Sunrise_Orsted_otn_metadata_tagging.csv") %>% clean_names()

tag_meta_files <- list.files("tag/tagmetadata", full.names = T)
tag_metadata <- vroom(tag_meta_files) 
tags <- tag_metadata %>% filter(Units == "Meters") %>% pull(`VUE Tag ID`)

# filter for only depth measurements 
# remove dogfish and a shark that died 
# create a column by array

dets <- detections %>%  
  left_join(shark_atag, by = c("transmitter_serial" = "tag_serial_number"), multiple = "all", relationship = "many-to-many") %>% #relationship = "many-to-many"
  filter(transmitter_id != "A69-9004-11623") %>% filter(common_name_e != "Smooth Dogfish") %>% 
  filter(!grepl("KIS|YB",station_name))

# set up the dataframe so we can run the false_detections() function
fdet <- dets %>% 
  #rename(detection_timestamp_utc = date_and_time_utc,
        #  receiver_sn = receiver,
       #   transmitter_id = transmitter) %>% 
  mutate(transmitter_codespace = tag_code_space,
         sensor_value = ifelse(sensor_value < 0, 0, sensor_value)) %>% 
  filter(transmitter_id %in% tags) %>% 
  filter(!is.na(sensor_value))

dets_a <- glatos::false_detections(fdet, tf = 3600) %>% filter(passed_filter == 1)



# Code for PSATS ----------------------------------------------------------


# List the Excel files in the folder
excel_files <- list.files(path = "DATA/psat", pattern = "\\.xlsx$", full.names = T)


# Set up the empty containers
df_list <- list()
combined_df_list <- list()


# Read in all the files
for (file in excel_files) {
  df <- read_excel(file)
  df_list[[file]] <- df 
}

# make sure we have all the files
length(df_list)

for (i in 1:length(df_list)) {
  # Clean the names 
  temp_df <- df_list[[i]] %>% clean_names()
  
  # Check if there's a datetime column; if not combine date and time columns
  # Also check that we change depth to pressure
  if (any(str_detect(colnames(temp_df), "(?=.*date)(?=.*time)"))) {
    temp_df <- temp_df %>% 
      mutate_at(vars(contains('dep')), abs) %>% 
      rename(date_time = matches("date.*"),
             temp = matches("temp"),
             press = matches("pres|dep")) %>% 
      mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y/%H:%M:%S")) %>% # weird datetime format
      select(date_time, temp, press)
  } else {
    temp_df <- temp_df %>% 
      mutate_at(vars(contains('dep')), abs) %>% 
      rename(temp = matches("temp"),
             press = matches("pres|dep")) %>%  
      mutate(date_time = as.POSIXct(paste(date, time), format ="%Y-%m-%d %H:%M:%S")) %>% 
      select(date_time, temp, press)
  }
  
  temp_df <- temp_df %>% 
    mutate(species = str_extract(names(df_list[i]), "(?<=_)([^_]+)"),
           tag_type = str_extract(names(df_list[i]), "(?<=/)([^_]+)"),
           tag_id = str_match(names(df_list[i]), ".*_([^_]+)\\.[^.]+$")[, 2],
           press = ifelse(press < 0, 0, press)) %>% 
    filter(press<200)
  
  
  print(paste("finished file", i, sep =  " "))
  
  combined_df_list[[i]] <- temp_df
} 


# Combine all data frames into one using bind_rows
dets_p <- bind_rows(combined_df_list)





