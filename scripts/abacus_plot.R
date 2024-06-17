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

