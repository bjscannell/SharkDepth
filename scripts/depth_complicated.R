source("https://raw.githubusercontent.com/bjscannell/lab_code/master/load_vue_csvs.R")


library(readxl)
library(lubridate)
library(dplyr)
library(janitor)


detections <- load_vue_files("detections")


tags <- read_excel("tag/shark_sn.xlsx", col_types = c("text", 
                                                  "text", "text"))



detections <- detections %>% 
  left_join(tags, by = "Transmitter Serial") %>% clean_names()


dets <- detections %>%  
  filter(sensor_unit == "m") %>% 
  filter(transmitter_x != "A69-9004-11623") %>% filter(species_commonname != "Smooth Dogfish") %>% 
  separate(station_name, "array") %>% 
  mutate(array = ifelse(substr(array,1,1) == "R", paste("SUN"), array))
  
det_lag <- detsbro %>% 
  rename(detection_timestamp_)


glatos::false_detections(detsbro)

  ggplot(detsbro,
         aes(x=species_commonname ,y=`Sensor Value`)) + 
            geom_point() + 
            scale_y_reverse() +
            facet_wrap(~array)


library(ggforce)
library(ggdist)  
library(gghalves)  
library(ggbeeswarm)  
  
  
  ggplot(detsbro, aes(x = species_commonname, y = `Sensor Value`)) + 
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
    geom_boxplot(
      width = .25, 
      outlier.shape = NA
    ) +
    geom_point(
      size = 1.5,
      alpha = .2,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) + 
    scale_y_reverse() +
    coord_cartesian(clip = "off") + 
    theme_minimal() +
    xlab("Species") +
    ylab("Depth") +
    ggtitle("Depth Distribution across Artificial Reefs")
  
  
detsbro %>% group_by(Transmitter.x) %>% 
    mutate(timediff = as.numeric(`Date and Time (UTC)` - lag(`Date and Time (UTC)`))) %>% 
    filter(!is.na(timediff)) %>% 
    select(`Date and Time (UTC)`, Receiver, Transmitter.x, `Sensor Value`  ,`Station Name`, Latitude, Longitude, species_commonname, timediff) %>%
    mutate(group_letter = letters[factor(`Sensor Value`)]) %>% View()
  
  

    
  
  
  ggplot(aes(x=`Date and Time (UTC)`, y = `Sensor Value`)) + geom_point() + scale_y_reverse() 

 

# one shark ---------------------------------------------------------------

  detsbro %>% filter(Transmitter.x == "A69-9004-11529") %>%
    mutate(timediff = as.numeric(`Date and Time (UTC)` - lag(`Date and Time (UTC)`)),
           `Sensor Value` = round(`Sensor Value`,1)) %>% 
    filter(!is.na(timediff)) %>% 
    select(`Date and Time (UTC)`, Receiver, Transmitter.x, `Sensor Value` 
           ,`Station Name`, Latitude, Longitude, species_commonname, timediff) %>%
             mutate(
               group = cumsum(`Sensor Value` != lag(`Sensor Value`, default = first(`Sensor Value`)) |
                                timediff > 300)) %>%
             group_by(group) %>%
             mutate(group_time_difference = max(`Date and Time (UTC)`) - min(`Date and Time (UTC)`),
                    group_time_difference = ifelse(group_time_difference == 0, 180, group_time_difference)) %>% ungroup() %>% 
    group_by(`Sensor Value`) %>% 
    summarise(sum = sum(group_time_difference)) %>% 
    ggplot(aes(x=`Sensor Value`, y = sum)) + 
    geom_bar(stat="identity") + coord_flip() + scale_x_reverse()
    
  

# by species --------------------------------------------------------------
# but this right now just sums up the total time per species
  
depth_bin <- detsbro %>% filter(Transmitter.x != "A69-9004-11623") %>% #dead
    group_by(Transmitter.x) %>%
    mutate(timediff = as.numeric(`Date and Time (UTC)` - lag(`Date and Time (UTC)`)),
           `Sensor Value` = round(`Sensor Value`,1)) %>% 
    filter(!is.na(timediff)) %>% 
    select(`Date and Time (UTC)`, Receiver, Transmitter.x, `Sensor Value` 
           ,`Station Name`, Latitude, Longitude, species_commonname, timediff) %>%
    mutate(
      group = cumsum(`Sensor Value` != lag(`Sensor Value`, default = first(`Sensor Value`)) |
                       timediff > 300)) %>%
    group_by(group,Transmitter.x) %>%
    mutate(group_time_difference = max(`Date and Time (UTC)`) - min(`Date and Time (UTC)`),
           group_time_difference = ifelse(group_time_difference == 0, 180, group_time_difference)) %>% ungroup() %>% 
    group_by(`Sensor Value`, species_commonname) %>% 
    summarise(sum = sum(group_time_difference)) 
  
  depth_bin %>% 
    ggplot(aes(x=`Sensor Value`, y = sum)) + 
    geom_bar(stat="identity") + coord_flip() + scale_x_reverse() + facet_wrap(~species_commonname)

  
  depth_bin %>% filter(species_commonname == "Smooth Dogfish") %>% View()

  
  
  detsbro %>% filter(species_commonname == "Dusky") %>% 
    ggplot(aes(x=`Date and Time (UTC)`, y= `Sensor Value`)) + geom_point() + scale_y_reverse() +facet_wrap(~Transmitter.x)
  
    

  # this shark died
  detections %>%  
    semi_join(tags, by = "Transmitter") %>% 
    left_join(fuck, by = "Transmitter Serial") %>% filter(Transmitter.x == "A69-9004-11623" & `Sensor Unit` == "m") %>% View()
    ggplot(aes(x=`Date and Time (UTC)`, y= `Sensor Value`)) + geom_point() + scale_y_reverse()
  
  
  detections %>%  
    semi_join(tags, by = "Transmitter") %>% 
    left_join(fuck, by = "Transmitter Serial") %>% filter(`Sensor Unit` == "m") %>% 
    ggplot(aes(x=`Date and Time (UTC)`, y= `Sensor Value`)) + geom_point() + scale_y_reverse() + facet_wrap(~Transmitter.x)
  
  
  
  

# pulling all the receivers that got sharks -------------------------------

detsbro %>% select(`Station Name`, Latitude, Longitude) %>% distinct(`Station Name`, .keep_all = T) %>% View()
  
detsbro %>% count(species_commonname, `Station Name`) %>% View()

detections %>% distinct(`Transmitter Serial`) %>% View()


SunriseWindTaggingData <- read_excel("tag/SunriseWindTaggingData.xlsx")


# number days detected ----------------------------------------------------


detsbro %>% 
  mutate(day = as.Date(`Date and Time (UTC)`)) %>% 
  group_by(`Transmitter Serial`) %>% distinct(day, .keep_all = T) %>% 
  summarise(n_days = n()) %>% View()


# number obs and depths ---------------------------------------------------


detsbro %>% group_by(`Transmitter Serial`) %>% 
  summarise(n_obs = n(),
            max_depth = max(`Sensor Value`),
            min_depth = min(`Sensor Value`),
            avg_depth = mean(`Sensor Value`)) %>% View()



# percentage above 2 ------------------------------------------------------
## also add 0 1

dets_a <- dets_a %>% arrange(detection_timestamp_utc) 

tags <- unique(dets_a$transmitter_id)

df <- data.frame()

for (i in 1:length(tags)) {

print(paste("working on tag", i, "of",length(tags)))
    
info <- dets_a %>% filter(transmitter_id == tags[i]) %>%
  mutate(timediff = as.numeric(detection_timestamp_utc - lag(detection_timestamp_utc)), #get the time difference between dets
         sensor_value = round(sensor_value,1)) %>%    #round sensor values
  filter(!is.na(timediff)) %>%  #gets rid of NAs
  select(detection_timestamp_utc, receiver_sn, transmitter_id, sensor_value 
         ,array, latitude, longitude, species_commonname, timediff) %>%
  mutate(
    group = cumsum(sensor_value != lag(sensor_value, default = first(sensor_value)) |
                     timediff > 300)) %>% #creates groups if by if time difference greater 300 or depth changes
  group_by(group) %>%
  mutate(group_time_difference = max(detection_timestamp_utc) - min(detection_timestamp_utc),
         group_time_difference = ifelse(group_time_difference == 0, 180, group_time_difference)) %>% ungroup() %>% 
  group_by(sensor_value) %>% 
  summarise(depth_time = sum(group_time_difference)) %>%
  summarise(sn = tags[i],
            total_time = sum(depth_time),
            time_above_2 = sum(depth_time[sensor_value < 2]),
            time_above_1 = sum(depth_time[sensor_value < 1]),
            percentage_time_above_2 = (time_above_2 / total_time) * 100,
            percentage_time_above_1 = (time_above_1 / total_time) * 100)

df <- rbind(df, info)
}

