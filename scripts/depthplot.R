library(dplyr)
library(readr)
library(janitor)
library(readxl)
library(ggplot2)
library(ggtext)
library(lubridate)
library(gganimate)
library(colorspace)
library(scattermore)


# depth plot --------------------------------------------------------------

pal <- c("#98867c", "#61516e", "#646468")
add_sample <- function(x){
  return(c(y = max(x) + .025, 
           label = length(x)))
}

num_ind <- function(x) {
  return()
}

dets_pa %>% 
  ggplot(aes(x = species, y = press)) +
  ggdist::stat_halfeye(
    aes(color = species,
        fill = after_scale(lighten(color,0.5))),
    adjust = 0.5,
    width = 0.75,
    .width = 0,
    justification = -.33,
    point_color = NA) +
  geom_boxplot(
    aes(color = species,
        #color = after_scale(darken(color, .1, space = "HLS")),
       fill = after_scale(desaturate(lighten(color, .8), .4))),
    width = .42, 
    outlier.shape = NA) +
  geom_scattermore(
    aes(color = species),
    #fill = "white",
    #shape = 21,
    #stroke = .4,
    #size = 2,
    alpha = 0.3,
    position = position_jitter(seed = 1, width = .12)) +
 # geom_point(
  #  aes(fill = species),
    #color = "transparent",
   # shape = 21,
    #stroke = .4,
    #size = 2,
    #alpha = .3,
    #position = position_jitter(seed = 1, width = .12)) +
  stat_summary(
    geom = "text",
    fun = "median",
    aes(label = round(after_stat(y), 1),
        color = species,
        color = after_scale(darken(color, .35, space = "HLS"))),
    #family = "Roboto Mono",
    fontface = "bold",
    size = 4.5,
    vjust = 0.6) +
  scale_y_reverse() +
  #scale_color_manual(values = pal, guide = "none") +
  #scale_fill_manual(values = pal, guide = "none") +
  scale_fill_discrete_qualitative() +
  scale_color_discrete_qualitative() +
  labs(
    x = NULL,
    y = "Depth (m)",
    title = "Depth Distribution of 9 Shark Species",
    subtitle = "Distribution of depths from sharks tagged with acoustic and PSAT tags." ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    axis.ticks = element_blank(),
    #axis.text.x = element_text(
     # color = (darken(pal, .1, space = "HLS")), 
      #size = 18),
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
  



# Individual depths -------------------------------------------------------
# truly a beautiful bar plot garden 

dets_pa %>% 
  arrange(species, tag_id) %>%  # Arrange by species, then tagID
  mutate(tag_id = factor(tag_id, levels = unique(tag_id))) %>%  # Reset factor levels to the order in the data frame
  ggplot() +
  geom_boxplot(aes(x = tag_id, y = press, 
                   color = species)) +
  theme_minimal() +
  scale_color_manual(values = c("#b30000", "#7c1158", "#4421af",
                                "#1a53ff", "#0d88e6", "#00b7c7",
                                "#5ad45a", "#8be04e", "#ebdc78")) +
  scale_y_continuous(breaks = seq(150,0, -25)) +
  theme(axis.text.x=element_blank()) 
  
  
