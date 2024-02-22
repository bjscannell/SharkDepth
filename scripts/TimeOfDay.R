library(suncalc)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

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


# Mirrored histogram ------------------------------------------------------

# pressure
df <- dets_pa_est_tod %>%
  mutate(press_bin = cut(press, breaks=seq(0, max(press), by=5), include.lowest=TRUE)) %>%
  group_by(tod, press_bin,tag_type, species) %>% 
  summarise(count = n()) %>%
  ungroup() %>% group_by(species, tag_type) %>% 
  mutate(percentage = count / sum(count) * 100)

# depth
df <- dets_pa_est_tod %>%
  mutate(depth = press*-1,
         depth_bin = cut(depth, breaks=seq(min(depth)-1, 0 , by=5), include.lowest=TRUE)) %>%
  group_by(tod, depth_bin,tag_type, species) %>% 
  summarise(count = n()) %>%
  ungroup() %>% group_by(species, tag_type) %>% 
  mutate(percentage = count / sum(count) * 100)



psat <- df %>% filter(tag_type == "psat/PSAT") %>% 
ggplot(aes(x = depth_bin, y = ifelse(tod == "Day", percentage, -percentage), fill = tod)) +
  geom_col() +
  scale_y_continuous(labels = abs, breaks = seq(20, -20, by = -5)) +
  coord_flip() +
  labs(x = "Depth", y = "Percentage of Detections", fill = "Time of Day", title = "PSAT") +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(hjust = 1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("Day" = "#ffde65", "Night" = "#1e3b7a")) +
  facet_wrap(~species, scales="free_y") 

acoustic <- df %>% filter(tag_type == "acoustic") %>% 
  ggplot(aes(x = depth_bin, y = ifelse(tod == "Day", percentage, -percentage), fill = tod)) +
  geom_col() +
  scale_y_continuous(labels = abs, breaks = seq(20, -20, by = -5)) +
  coord_flip() +
  labs(x = "Depth", y = "Percentage of Detections", fill = "Time of Day", title = "Acoustic") +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(hjust = 1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("Day" = "#ffde65", "Night" = "#1e3b7a")) +
  facet_wrap(~species, scales="free_y")

psat + acoustic



# Diel detections ---------------------------------------------------------

df <- dets_pa_est_tod %>% 
  mutate(hour = hour(date_time),
          press = ifelse(press <= 0, 0.01, press))

kruskal_result <- kruskal.test(press ~ , data = dets_pa_est_tod)

m0<- glm(press ~ hour, data=df, family = Gamma(link = log))

summary(m0)

m1<- glmer(press ~ hour*species + (1|tag_id), 
           family = Gamma(link = log), data = df, 
           glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))

