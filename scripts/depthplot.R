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
library(cowplot)


# depth plot --------------------------------------------------------------

pal <- c("#98867c", "#61516e", "#646468")
add_sample <- function(x){
  return(c(y = max(x) + .025, 
           label = length(x)))
}

num_ind <- function(x) {
  return()
}

x <- dets_pa %>% 
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
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white"),
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
    plot.margin = margin(15, 15, 10, 15)) 


  ggsave("plots/full_depth_distr.png",x, dpi = 360, width = 20, height = 9, units = "in")
  



# Individual depths -------------------------------------------------------
# truly a beautiful bar plot garden 

species_depth <- dets_pa %>% 
  mutate(thresher_white = ifelse(species == "Thresher" | species == "White", 1,0)) %>% 
  arrange(species, tag_id) %>% 
  mutate(tag_id = factor(tag_id, levels = unique(tag_id))) 

all <- ggplot(filter(species_depth, thresher_white == 0)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = .8, color = "#009688") +
  geom_hline(yintercept = 3, color = "#762a83", linetype = "dashed",  size = .8) +
  geom_boxplot(aes(x = tag_id, y = press), 
                   fill = "grey", outlier.shape = NA) +
  theme_classic(base_size=16) +
  theme(axis.text.x=element_blank(),
        legend.position = "none") +
  #coord_cartesian(ylim=c(40, 0)) 
  ylim(c(30,0)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white")) 
  
  
thresher_white <- ggplot(filter(species_depth, thresher_white == 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = .8,  color = "#009688") +
  geom_hline(yintercept = 3, color = "#762a83", linetype = "dashed",  size = .8) +
  geom_boxplot(aes(x = tag_id, y = press), 
                   fill = "grey",outlier.shape = NA) +
  theme_classic(base_size=16) +
  scale_y_reverse(breaks = seq(0,150, 25), position = "right") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white")) 
  
  

x <- plot_grid(all, thresher_white, ncol = 2, rel_widths = c(3, 1))

ggsave("plots/ind_depth_box.png", x, dpi = 360, width = 15, height = 9, units = "in")

label_data <- species_depth %>%
  filter(thresher_white == 0) %>%
  group_by(species) %>%
  summarize(min_tag_id = min(as.numeric(tag_id)), 
            max_tag_id = max(as.numeric(tag_id)), 
            .groups = 'drop') %>%
  mutate(mid_point = (min_tag_id + max_tag_id) / 2) # Calculate the midpoint for label placement

ggplot(filter(species_depth, thresher_white == 0)) +
  geom_boxplot(aes(x = tag_id, y = press, color = species), outlier.shape = NA) +
  geom_text(data = label_data, 
            aes(x = mid_point, y = 20, label = species), 
            color = "black", hjust = 0.5, vjust = -2) + # Adjust text color and position
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        legend.position = "none") +
  coord_cartesian(ylim=c(40, 0))
