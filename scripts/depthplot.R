library(dplyr)
library(readr)
library(janitor)
library(readxl)
library(ggplot2)
library(lubridate)
library(gganimate)

dets <- read_csv("Desktop/sunrise.csv") %>% clean_names() %>% 
  mutate(date_and_time_utc = as_datetime(date_and_time_utc))
         

tags <- read_excel("Desktop/sharks.xlsx") %>% clean_names()


shark_dets <- dets %>% filter(transmitter %in% tags$transmitter_id)


rec_int <- read_excel("Desktop/rec_id.xlsx") %>% clean_names()



det_filt <- dets %>% 
  filter(!transmitter %in% rec_int$transmit_id)


full_shark_dets <- shark_dets %>% left_join(tags, by = c("transmitter" = "transmitter_id")) %>% 
  mutate(sensor_value_cal = ifelse(as.integer(substring(transmitter, nchar(transmitter))) %% 2 == 0,
                                   sensor_value*0.1575-5.157,
                                   sensor_value*0.302-1.2129),
         hour = hour(with_tz(date_and_time_utc, "EST")),
         daytime = ifelse(hour >= 6 & hour <= 19, "day", "night"))



full_shark_dets %>%  
  group_by(tag_serial_number) %>% 
  mutate(det_count = n()) %>% 
  distinct(tag_serial_number, .keep_all = T) %>% View()


base <- full_shark_dets %>% 
  filter(transmitter =="A69-9004-11538" | transmitter =="A69-9004-11539")
 

full_shark_dets %>% 
  filter(transmitter =="A69-9004-11538" | transmitter =="A69-9004-11539") %>% 
  ggplot(aes(x=date_and_time_utc, y = station_name)) +
  geom_point()


st <- full_shark_dets %>% 
  filter(transmitter =="A69-9004-11538" | transmitter =="A69-9004-11539") %>% 
  mutate(time_diff = date_and_time_utc-lag(date_and_time_utc),
         time_diff = ifelse(is.na(time_diff), 0, time_diff),
         direction = lag(longitude)-longitude,
         new_event = ifelse(time_diff >3600, 1, 0),
         sum = cumsum(new_event)) %>% 
  group_by(sum) %>% 
  summarize(difference = last(d_2)-first(d_2))
st %>%  ggplot(aes(y = direction, x = common_name_e)) + geom_point()

st %>% ggplot(aes(x=date_and_time_utc, y = time_diff)) + geom_line() + geom_point() +
  ylim(0,3600)




# depth plot --------------------------------------------------------------

pal <- c("#98867c", "#61516e", "#646468")
add_sample <- function(x){
  return(c(y = max(x) + .025, 
           label = length(x)))
}

num_ind <- function(x) {
  return()
}

full_shark_dets %>% 
  filter(tag_sensor_type == "Depth") %>% 
  ggplot(aes(x = common_name_e, y = sensor_value_cal)) +
  ggdist::stat_halfeye(
    aes(color = common_name_e,
        fill = after_scale(lighten(color,0.5))),
    adjust = 0.5,
    width = 0.75,
    .width = 0,
    justification = -.33,
    point_color = NA) +
  geom_boxplot(
    aes(color = common_name_e,
        color = after_scale(darken(color, .1, space = "HLS")),
        fill = after_scale(desaturate(lighten(color, .8), .4))),
    width = .42, 
    outlier.shape = NA) +
  geom_point(
    aes(color = common_name_e,
        color = after_scale(darken(color, .1, space = "HLS"))),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 2,
    position = position_jitter(seed = 1, width = .12)) +
  geom_point(
    aes(fill = common_name_e),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 2,
    alpha = .3,
    position = position_jitter(seed = 1, width = .12)) +
  scale_y_reverse() +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  labs(
    x = NULL,
    y = "Depth (m)",
    title = "Depth Distribution of 3 Large Coastal Sharks",
    subtitle = "Distribution of depths from acoustically tagged sharks within the Sunrise VPS array." ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      color = (darken(pal, .1, space = "HLS")), 
      size = 18),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    plot.title = element_markdown(face = "bold", size = 21),
    plot.subtitle = element_text(
      color = "grey40", hjust = 0,
      margin = margin(0, 0, 20, 0)
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      color = "grey40", lineheight = 1.2,
      margin = margin(20, 0, 0, 0)),
    plot.margin = margin(15, 15, 10, 15)
  )
  



# abacus ------------------------------------------------------------------


full_shark_dets %>%
  filter(as.integer(substring(transmitter, nchar(transmitter))) %% 2 == 0) %>% 
  ggplot(aes(x=date_and_time_utc, y = transmitter)) + geom_point()


