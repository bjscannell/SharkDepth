
library(ggplot2)
library(dplyr)
library(stringr)


# just a peak -------------------------------------------------------------

agg_df <- dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  group_by(tag_id) %>% 
  mutate(median_press = median(press),
         median_press = ifelse(median_press <= 0, 0.01, median_press)) %>% 
  distinct(tag_id, .keep_all = T) %>% ungroup()

fittedgModel <- glm(median_press ~ tag_type + species, 
                     family = Gamma(link = "log"), data = agg_df)


kruskal.test(median_press ~ tag_type, data = agg_df)



testDispersion(fittedModel)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F)
residuals(simulationOutput)
plot(simulationOutput)



# summary stats -----------------------------------------------------------

stats_df <- agg_df %>%
  group_by(species) %>%
  summarise(
    MedianDepth = median(press),
    Count = n(),
    SD = sd(press)
  )


ggplot() +
  geom_boxplot(data=agg_df,aes(x=species,y=median_press),outlier.shape = NA) +
  geom_text(data=stats_df, 
            aes(label = paste(round(MedianDepth,2)),
                x = species, y = MedianDepth), vjust = 3, size =3) +
  geom_text(data=stats_df, 
            aes(label = paste("\nN:", Count),
                x = species, y = 125), vjust = 2.5, size =3) +
  scale_x_discrete(labels = function(x) str_replace_all(x, " ", "\n")) +
  scale_y_reverse() +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(t = 10, r = 10, b = 50, l = 10, unit = "pt"),
        axis.title.x = element_text(margin = margin(t = 25, b = -20)),
        axis.text.x = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),) +
  coord_cartesian(clip="off") +
  labs(title = "Distribution of Median Depth by Species", x = "Species", y = "Median Depth")






# check normality ---------------------------------------------------------

# Shapiro-Wilk test for normality by species
library(broom)
agg_df %>%
  filter(species!= "Blacktip" & species!= "Tiger") %>% 
  group_by(species) %>%
  do(tidy(shapiro.test(.$median_press)))

# Levene's test for homogeneity of variances
library(car)
leveneTest(median_press ~ species, data = agg_df)


kruskal_result <- kruskal.test(median_press ~ species, data = agg_df)
print(kruskal_result)

library(rstatix)
dunn_result <-dunn_test(median_press ~ species, data=agg_df, p.adjust.method ="bonferroni") 
print(dunn_result) 


# Assuming 'dunn_result' is your dataframe
library(ggplot2)
library(reshape2)

# Convert to a wide format where each cell represents the p-value between species
p_matrix <- dcast(dunn_result, group1 ~ group2, value.var = "p.adj")

# Replace NA with 1 for non-comparisons
p_matrix[is.na(p_matrix)] <- 1

# Ordering matrix rows and columns by species names might help in readability
species_order <- sort(unique(c(dunn_result$group1, dunn_result$group2)))
p_matrix <- p_matrix[match(species_order, p_matrix$group1), match(species_order, names(p_matrix))]

# Plotting heatmap
heatmap_data <- melt(p_matrix, id.vars = "group1") %>% 
  mutate(sig = ifelse(value>0.05, 1, 0))

ggplot(heatmap_data, aes(x=variable, y=group1, 
                         fill=sig)) +
  geom_tile() +
  geom_text(data=filter(heatmap_data, sig == "0"), 
            aes(label = round(value,5),
                x = variable, y = group1)) +
  scale_fill_gradient(low = "red", high = "white", limits = c(0, 1), name = "Adj. P-Value") +
  scale_x_discrete(labels = function(x) str_replace_all(x, " ", "\n")) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(x = "Species", y = "Species", title = "Heatmap of Adjusted P-Values for Depth Comparisons") 

