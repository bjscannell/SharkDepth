
# Code for Acoustics ------------------------------------------------------

dets_a <- dets_a %>% 
  group_by(transmitter_id) %>% 
  filter(detection_timestamp_utc > (min(detection_timestamp_utc) + hours(12))) %>%  # remove the first 12 hours for each tag
  ungroup() %>% 
  filter(month(detection_timestamp_utc) >= 6 & month(detection_timestamp_utc) <= 9)   # filter for June to September

# species
dets_a %>% 
  group_by(common_name_e) %>% 
  mutate(above1 = ifelse(sensor_value <1, 1,0),
         above2 = ifelse(sensor_value <2, 1, 0)) %>% 
  summarise(day_count = n_distinct(date(detection_timestamp_utc)),
            n_ind = n_distinct(transmitter_id),
            n_obs = n(),
            min_dep = min(sensor_value),
            max_dep = max(sensor_value),
            mean_dep = mean(sensor_value),
            per_above2 = sum(above2/length(above2)),
            per_above1 = sum(above1/length(above1))) %>% 
  write_csv("output/Acoustic_SpeciesSummary.csv")


# individual
dets_a %>% 
  group_by(transmitter_id) %>% 
  mutate(above1 = ifelse(sensor_value <1, 1,0),
         above2 = ifelse(sensor_value <2, 1, 0),
         above3 = ifelse(sensor_value <3, 1, 0)) %>% 
  summarise(day_count = n_distinct(date(detection_timestamp_utc)),
            n_obs = n(),
            min_dep = min(sensor_value),
            max_dep = max(sensor_value),
            mean_dep = mean(sensor_value),
            per_above3 = sum(above3/length(above3)),
            per_above2 = sum(above2/length(above2)),
            per_above1 = sum(above1/length(above1))) %>% 
  write_csv("output/Acoustic_IndividualSummary.csv")




# Code for PSATS ----------------------------------------------------------

dets_p <- dets_p %>% 
  group_by(tag_id) %>% 
  filter(date_time > min(date_time) + hours(12)) %>%  # remove the first 12 hours for each tag
  ungroup() %>% 
  filter(month(date_time) >= 6 & month(date_time) <= 9) %>%  # filter for June to September
  filter(press < 150) %>%  # filter out the incorrect pressures
  mutate(press = ifelse(press < 0, 0, press)) %>%  # make neg pressures 0
  group_by(tag_id, date_time) %>% # remove duplicate date_times
  distinct() 

# individual summary
dets_p %>% 
  group_by(tag_id) %>% 
  mutate(above1 = ifelse(press <1, 1,0),
         above2 = ifelse(press <2, 1, 0),
         above3 = ifelse(press <3, 1, 0)) %>% group_by(tag_id) %>% 
  summarise(day_count = n_distinct(date(date_time)),
            #n_ind = n_distinct(transmitter_id),
            n_obs = n(),
            min_dep = min(press),
            max_dep = max(press),
            mean_dep = mean(press),
            median_depth = median(press),
            per_above2 = sum(above2/length(above2)),
            per_above1 = sum(above1/length(above1)),
            per_above3 = sum(above3/length(above3))) %>%
  write_csv("output/PSAT_IndividualSummary.csv")


# species summary
dets_p %>% ungroup() %>% 
  group_by(species) %>% 
  mutate(above1 = ifelse(press <1, 1,0),
         above2 = ifelse(press <2, 1, 0)) %>% 
  summarise(day_count = n_distinct(date(date_time)),
            n_ind = n_distinct(tag_id),
            n_obs = n(),
            min_dep = min(press),
            max_dep = max(press),
            mean_dep = mean(press),
            per_above2 = sum(above2/length(above2)),
            per_above1 = sum(above1/length(above1))) %>% 
  write_csv("output/PSAT_SpeciesSummary.csv")




# prep for suncalc --------------------------------------------------------

# change acoustic data to fit PSAT format

dets_p <- dets_p %>% select(-temp) 

dets_pa <- dets_a %>% 
  rename(date_time = detection_timestamp_utc,
         tag_id = transmitter_id,
         species = common_name_e,
         press = sensor_value) %>% 
  mutate(tag_type = "acoustic") %>% 
  select(date_time, press, species, tag_type, tag_id) %>% 
  rbind(dets_p)




# get full species summaries ------------------------------------------------------

dets_pa %>% ungroup() %>% 
  group_by(species) %>% 
  mutate(above1 = ifelse(press <1, 1,0),
         above2 = ifelse(press <2, 1, 0),
         above3 = ifelse(press <3, 1,0)) %>% 
  summarise(day_count = n_distinct(date(date_time)),
            n_ind = n_distinct(tag_id),
            n_obs = n(),
            min_dep = min(press),
            max_dep = max(press),
            mean_dep = mean(press),
            per_above3 = sum(above3/length(above3)),
            per_above2 = sum(above2/length(above2)),
            per_above1 = sum(above1/length(above1))) %>% 
  write_csv("output/Full_SpeciesSummary.csv")


