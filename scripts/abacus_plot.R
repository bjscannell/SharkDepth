# abacus ------------------------------------------------------------------



x <- dets_pa %>% 
  group_by(tag_id) %>% 
  mutate(date = as_date(date_time)) %>%
  distinct(date, .keep_all = T) %>% ungroup()  %>% 
  arrange(species, tag_id) %>%  # Arrange by species, then tagID
  mutate(tag_id = factor(tag_id, levels = unique(tag_id)))  # Reset factor levels to the order in the data frame



ggplot(x, aes(x = date, y = tag_id,
              color = species, shape = tag_type)) +
  geom_point(size = 1) +
  scale_shape_manual(values = c(1, 4)) + 
  scale_color_discrete_qualitative() +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank() , 
    axis.ticks.y = element_blank(),
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

