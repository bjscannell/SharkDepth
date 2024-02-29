



library(ggplot2)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(broom)
library(car)
library(rstatix)
library(lme4)


# acoustic vs psat --------------------------------------------------------

kruskal_result <- kruskal.test(median_press ~ tag_type, data = agg_df)

# summary stats -----------------------------------------------------------
agg_df <- dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  group_by(tag_id) %>% 
  mutate(median_press = median(press),
         median_press = ifelse(median_press <= 0, 0.01, median_press)) %>% 
  distinct(tag_id, .keep_all = T) %>% ungroup() %>% 
  mutate(thresher = ifelse(species == "Thresher", 1,0)) %>% ungroup()

stats_df <- agg_df %>%  
  group_by(species) %>%
  summarise(
    MedianDepth = median(median_press),
    Count = n(),
    SD = sd(press))


ggplot() +
  geom_boxplot(data=agg_df,
               aes(x=species,y=median_press),outlier.shape = NA) +
  geom_text(data=stats_df,
            aes(label = paste(round(MedianDepth,2)),
                x = species, y = MedianDepth), vjust = 3, size =3) +
  geom_text(data=stats_df,
            aes(label = paste("\nN:", Count),
                x = species, y = 30), vjust = 2.8, size =3) +
  scale_x_discrete(labels = function(x) str_replace_all(x, " ", "\n")) +
  scale_y_reverse(limits = c(30, 0)) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(t = 10, r = 10, b = 50, l = 10, unit = "pt"),
        axis.title.x = element_text(margin = margin(t = 25, b = -20)),
        axis.text.x = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),) +
  coord_cartesian(clip="off") +
  labs(title = "Distribution of Median Depth by Species", x = "Species", y = "Median Depth")


# Non parametric ---------------------------------------------------------

# Shapiro-Wilk test for normality by species
agg_df %>%
  filter(species!= "Blacktip" & species!= "Tiger") %>% 
  group_by(species) %>%
  do(tidy(shapiro.test(.$median_press)))

# Levene's test for homogeneity of variances

leveneTest(median_press ~ species, data = agg_df)

# non-parametric group differences
kruskal_result <- kruskal.test(median_press ~ species, data = agg_df)
print(kruskal_result)

# look at differences per group
dunn_result <-dunn_test(median_press ~ species, data=agg_df, p.adjust.method ="bonferroni") 
print(dunn_result) 


# Make a plot 
# Convert to a wide format where each cell represents the p-value between species
p_matrix <- dcast(dunn_result, group1 ~ group2, value.var = "p.adj")

# Replace NA with 1 for non-comparisons
p_matrix[is.na(p_matrix)] <- 1

# Ordering matrix rows and columns by species names might help in readability
species_order <- sort(unique(c(dunn_result$group1, dunn_result$group2)))
#p_matrix <- p_matrix[match(species_order, p_matrix$group1), match(species_order, names(p_matrix))]

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


# Time above 3m -----------------------------------------------------------

df3m <- dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  mutate(above3 = ifelse(press<3,1,0)) %>% 
  group_by(tag_id) %>% 
  mutate(count = n()) %>% ungroup() %>% 
  mutate(species = as.factor(species))

ggplot(df3m) +
  geom_point(aes(x=tag_id, y = count)) +
  scale_y_log10()

df3mI <- dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  mutate(above3 = ifelse(press<3,1,0))  %>% 
  group_by(tag_id) %>% 
  summarize(
    total_count = n(),
    above3_count = sum(above3),
    percent_above3 = (above3_count / total_count) 
  ) #%>% filter(total_count > 100)

df3mS <-dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  mutate(above3 = ifelse(press<3,1,0))  %>% 
  group_by(species) %>% 
  summarize(
    total_count = n(),
    above3_count = sum(above3),
    percent_above3 = (above3_count / total_count) 
  ) #%>% filter(total_count > 100)


ggplot(df3mS,
       aes(x=reorder(species, percent_above3), y=percent_above3, fill=species)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Proportion of Time Spent in Top 3 Meters by Shark Species", x="Species", y="Proportion") +
  coord_flip() 

ggplot(df3mI,
       aes(x=reorder(tag_id, percent_above3), y=percent_above3)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Proportion of Time Spent in Top 3 Meters by Shark Species", x="Species", y="Proportion") +
  coord_flip() 

df3m$species <- relevel(df3m$species, ref = "Dusky")



# Adding Random effect ----------------------------------------------------
# Ideas:
# get rid of low n species
# remove "" from around binomial


# glmre -------------------------------------------------------------------
df3m <- read_csv("df3m.csv")
small <- c("Thresher", "Blacktip", "Tiger", "Smooth Hammerhead")
# excluding these we get 1416435 rows
small_df <- df3m %>% 
  filter(! species %in% small) %>% 
  sample_n(20000)  %>% 
  mutate(species = as.factor(species),
         tag_id = as.factor(tag_id))



# m5 <- MCMCglmm(above3~species,
#                random=~tag_id,data=small_df,
#                family="categorical",
#                verbose=FALSE)
# 
# m6 <- MCMCglmm(above3~species - 1,
#                random=~tag_id,data=df3m,
#                family="categorical",
#                verbose=FALSE)

summary(m6)
model_summary <- read_txt("/Users/brittneyscannell/Desktop/tobey/data/summary_m6.txt")
my_data <- read.delim("output/summary_m6.txt")
posterior_means <- read_csv("output/posterior_means.csv")
posterior_means <- m6$Sol

par(mfrow=c(9,2))
plot(m6$Sol, auto.layout=T)

posterior_samples <- as.data.frame(m6$Sol)

posterior_long <- posterior_means %>% 
  as.data.frame() %>%
  rownames_to_column("Parameter") %>%
  pivot_longer(-Parameter, names_to = "Iteration", values_to = "Value") #%>% 
#mutate(Value = exp(Value))


ggplot(posterior_long, aes(y = Iteration, x = Value)) +
  stat_halfeye() +
  theme_minimal() +
  labs(title = "Posterior Distributions with Credible Intervals",
       x = "Value",
       y = "Parameter")



species_effects_df <- data.frame(posterior_means) %>%
  pivot_longer(cols = everything(), names_to = "species", values_to = "posterior_draw") %>% 
  mutate(p = exp(posterior_draw)/(1+exp(posterior_draw))) %>% 
  group_by(species) %>% 
  summarise(p_mean = mean(p),
            cilo = quantile(p, probs = 0.025),
            cihi = quantile(p, probs = 0.975))


# species_effects_df <- data.frame(posterior_means) %>%
#   pivot_longer(cols = everything(), names_to = "species", values_to = "posterior_draw") %>% 
#   mutate(p_mean = exp(posterior_draw)/(1+exp(posterior_draw))) 


# ggplot(df, aes(x=p)) +
#   geom_histogram() +
#   facet_wrap(~species)



ggplot(species_effects_df, aes(x = species, y = p_mean)) +
  geom_point(fill = "skyblue") +
  geom_errorbar(aes(ymin = cilo, ymax = cihi), width = 0.2) +
  labs(title = "Predicted Probability of 'Above3' by Species",
       x = "Species",
       y = "Predicted Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


