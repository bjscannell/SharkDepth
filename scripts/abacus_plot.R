# abacus ------------------------------------------------------------------
library(colorspace)
library(ggtext)


x <- dets_pa %>% 
  group_by(tag_id) %>% 
  mutate(date = as_date(date_time)) %>%
  distinct(date, .keep_all = T) %>% ungroup()  %>% 
  arrange(species, tag_id) %>%  # Arrange by species, then tagID
  mutate(tag_id = factor(tag_id, levels = unique(tag_id)))  # Reset factor levels to the order in the data frame



x <- ggplot(x, aes(x = date, y = tag_id,
              color = tag_type), size = 2, shape =21) +
  geom_point() +
  labs(title = "",
       x = "Date",
       y = "Individual",
       color = "Tag Type") +
  scale_color_manual(labels = c("Acoustic", "PSAT"), values = c("#4c72b0", "#dd8452")) +
  theme_classic(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank() , 
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white"))


ggsave("plots/abacus.png", x, dpi = 360, width = 15, height = 8, units = "in")


# tracking day plot -------------------------------------------------------

df <- dets_pa %>% 
  mutate(week = as.factor(isoweek(date_time)),
         date = date(date_time)) %>% 
  group_by(week) %>% 
  summarise(n = n_distinct(date)) 


ggplot(df) +
  geom_point(aes(x = week, y = n)) +
  theme_classic(base_size = 20) +
  theme(
    axis.text.x = element_text(face="bold",
                               angle = 0,
                               margin = margin(t = 25),
                               size = 17),
    axis.text.y = element_text(size = 17),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    legend.position = c(0.1, 0.98),
    legend.title = element_blank(),
    #legend.justification = c("left", "top"),
    legend.direction = "horizontal",
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white")) 
