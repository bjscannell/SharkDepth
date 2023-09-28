# libraries
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(readr)

source("https://raw.githubusercontent.com/bjscannell/lab_code/master/load_vue_csvs.R")



# Code for Acoustics ------------------------------------------------------


detections <- load_vue_files(project = "detect")

tags <- read_excel("tag/tags.xlsx") 

shark_sn <- read_excel("tag/shark_sn.xlsx", col_types = c("text", 
                                                          "text", "text"))

# filter for only depth measurements 
# remove dogfish and a shark that died 
# create a column by array

dets <- detections %>%  
  semi_join(tags, by = "Transmitter") %>% 
  left_join(shark_sn, by = "Transmitter Serial") %>% filter(`Sensor Unit` == "m") %>% 
  filter(Transmitter.x != "A69-9004-11623") %>% filter(species_commonname != "Smooth Dogfish") %>%  
  separate(`Station Name`, "array") %>% 
  mutate(array = ifelse(substr(array,1,1) == "R", paste("SUN"), array)) %>% clean_names()

# set up the dataframe so we can run the false_detections() function
fdet <- dets %>% 
  filter(array != "YBAR" | array != "KIS") %>%  # remove inshore artificial reefs
  rename(detection_timestamp_utc = date_and_time_utc,
         receiver_sn = receiver,
         transmitter_id = transmitter_x) %>% 
  mutate(transmitter_codespace = "A69-9004",
         sensor_value = ifelse(sensor_value < 0, 0, sensor_value))

dets_a <- glatos::false_detections(fdet, tf = 3600) %>% filter(passed_filter == 1)



# Code for PSATS ----------------------------------------------------------


# List the Excel files in the folder
excel_files <- list.files(path = "DATA", pattern = "\\.xlsx$", full.names = T)


# Set up the empty containers
df_list <- list()
combined_df_list <- list()


# Read in all the files
for (file in excel_files) {
  df <- read_excel(file, sheet = "Sheet2")
  df_list[[file]] <- df 
}

# make sure we have all the files
length(df_list)

for (i in 1:length(df_list)) {
  # Clean the names 
  temp_df <- df_list[[i]] %>% clean_names()
  
  # Check if there's a datetime column; if not combine date and time columns
  if (any(str_detect(colnames(temp_df), "date_"))) {
    temp_df <- temp_df %>% 
      rename(date_time = matches("date_"),
             temp = matches("temp"),
             press = matches("pres")) %>% 
      mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y/%H:%M:%S")) %>% # weird datetime format
      select(date_time, temp, press)
  } else {
    temp_df <- temp_df %>% 
      rename(temp = matches("temp"),
             press = matches("pres")) %>%  
      mutate(date_time = as.POSIXct(paste(date, time), format ="%Y-%m-%d %H:%M:%S")) %>% 
      select(date_time, temp, press)
  }
  
  temp_df <- temp_df %>% 
    mutate(species = str_extract(names(df_list[i]), "(?<=_)([^_]+)"),
           tag_type = str_extract(names(df_list[i]), "(?<=/)([^_]+)"),
           tag_id = str_match(names(df_list[i]), ".*_([^_]+)\\.[^.]+$")[, 2])
  
  print(paste("working on file", i, sep =  " "))
  
  combined_df_list[[i]] <- temp_df
} 


# Combine all data frames into one using bind_rows
dets_p <- bind_rows(combined_df_list)



# Code for the CATS Cam ---------------------------------------------------

list.files("DATA", pattern = "CATS")

