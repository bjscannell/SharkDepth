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

species_depth <- agg_df %>% 
  mutate(thresher_white = ifelse(species == "Thresher" | species == "White" | species == "Tiger", 1,0))

species_count <- species_depth %>% group_by(species) %>%  summarize(count = n()) %>% arrange(count)  

species_depth <-species_depth %>% 
  arrange(species, tag_id) %>% 
  mutate(species = factor(species, levels = species_count$species),
         tag_id = factor(tag_id, levels = unique(tag_id))) %>% ungroup() %>% group_by(species) %>% 
  mutate(species_count = n()) %>% 
  arrange(species_count)




all <- ggplot(filter(species_depth, thresher_white == 0)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = .8, color = "#009688") +
  geom_hline(yintercept = 3, color = "#762a83", linetype = "dashed",  size = .8) +
  geom_segment(aes(x = reorder(tag_id, species_count), y = quant10,
                   yend = quant90, xend = reorder(tag_id, species_count)),
               color = "grey38",
               size = 0.8,
               lineend='round') +
  geom_point(aes(x = reorder(tag_id, species_count), y = median_press, color = species), size = 4) +
  geom_point(aes(x = reorder(tag_id, species_count), y = median_press), shape = 1,size = 4 ,colour = "black") +
  annotate("text",
           x = c(1.5, 1.5),
           y = c(1, 3),
           label = c("1", "3"),
           color = c( "#009688","#762a83"),
           size=6, 
           hjust = 2.8) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_classic(base_size=20) +
  scale_y_reverse(breaks = seq(0,30, 5), position = "left") +
  scale_x_discrete(labels=filter(species_depth, thresher_white == 1)$tag_id) +
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#ffff33", "#ff7f00", "#984ea3")) +
  labs(title="",
       x ="Individual", y = "Depth (m)") +
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(t = 2, r = 5, b = 21, l = 5, unit = "pt"),
        axis.title.x = element_text(hjust = 0.7, vjust = -0.1, size = 26),
        axis.text.x=element_blank(),
        axis.text.y = element_text(size = 17),
        axis.title.y = element_text(size = 26),
        legend.title = element_blank(),
        legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white")) 


thresher_white <- ggplot(filter(species_depth, thresher_white == 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = .8,  color = "#009688") +
  geom_hline(yintercept = 3, color = "#762a83", linetype = "dashed",  size = .8) +
  geom_segment(aes(x = reorder(tag_id, species_count), y = quant10,
                   yend = quant90, xend = reorder(tag_id, species_count)),
               color = "grey38",
               size = 0.8,
               lineend='round') +
  geom_point(aes(x = reorder(tag_id, species_count), y = median_press, color = species), size = 4) +
  geom_point(aes(x = reorder(tag_id, species_count), y = median_press), shape = 1,size = 4, colour = "black") +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_classic(base_size=20) +
  scale_x_discrete(labels=filter(species_depth, thresher_white == 1)$tag_id)+
  scale_y_reverse(breaks = seq(0,150, 25), position = "left") +
  scale_colour_manual(values = c("#a65628", "#f781bf", "#999999")) +
  theme(plot.margin = margin(t = 35, r = 5, b = 51, l = 5, unit = "pt"),,
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size = 17),
        legend.title = element_blank(),
        legend.position = c(0.45, 1),
        legend.direction = "horizontal",
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white")) 



y <- plot_grid(all, thresher_white, ncol = 2, rel_widths = c(3, 1))

ggsave("plots/ind_depth_box.png", y, dpi = 360, width = 15, height = 9, units = "in")

