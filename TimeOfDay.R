library(suncalc)
library(dplyr)
library(lubridate)

dets_pa_est <- dets_pa %>% 
  mutate(date_est = as.Date(with_tz(date_time, tzone = "EST")),
         date_times_est = with_tz(date_time, tzone = "EST")) 

longislandSUN <- getSunlightTimes(distinct(dets_pa_est, date_est)$date_est,
                                  lat = 40.65,
                                  lon = -72.97,
                                  keep = c("sunrise", "sunset"),
                                  tz = "EST")

dets_pa_est_tod <- dets_pa_est %>% 
  left_join(longislandSUN, by = c("date_est" = "date")) %>%
  mutate(tod = ifelse(date_times_est >= sunrise & date_times_est < sunset, 'Day', 'Night')) %>% 
  select(-c(lat, lon, sunrise, sunset))


dets_pa_est_tod %>% 
  ggplot(aes(x = as.factor(tod), y = press,
             fill = tod)) +
  geom_boxplot() +
  scale_y_reverse() +
  facet_wrap(~ species, nrow = 1, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "cm"),
        strip.placement = "outside") +
  theme(panel.spacing = unit(0, "cm"),
        strip.placement = "outside") +
  scale_fill_manual(values = c("#ffde65","#1e3b7a")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 14),
        panel.spacing = unit(0, "cm"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20,
                                  margin = margin(b = 10)),
        plot.subtitle = element_text(lineheight = 1.1,
                                     margin = margin(b = 20),
                                     size = 14,
                                     color = "grey30"),
        plot.caption.position = "plot",
        plot.caption = element_text(color = "grey50",
                                    hjust = 0)) +
  labs(x = "", y = "Depth (m)",
       fill = "",
       title = "Depth Distribution by time of day ")
