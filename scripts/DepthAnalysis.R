
library(ggplot2)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(broom)
library(car)
library(rstatix)



# summary stats -----------------------------------------------------------
agg_df <- dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  group_by(tag_id) %>% 
  mutate(median_press = median(press),
         median_press = ifelse(median_press <= 0, 0.01, median_press)) %>% 
  distinct(tag_id, .keep_all = T) %>% ungroup()

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


m0<- glm(above3 ~ species, data=df3m, family=binomial)


coef_estimates <- coef(summary(m0))
odds_ratios <- exp(coef_estimates[, "Estimate"])
conf_int <- exp(confint.default(m0)) # calculate confidence intervals


species_names <- rownames(coef_estimates)
df_plot <- data.frame(species = species_names, odds_ratio = odds_ratios,
                      lower = conf_int[,1], upper = conf_int[,2])

ggplot(df_plot, aes(x = reorder(species, -odds_ratio), y = odds_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() + # Flip coordinates for horizontal layout
  labs(title = "Odds Ratios of Shark Species Being Observed Above 3 Meters",
       x = "Species", y = "Odds Ratio") +
  theme_minimal()

# Predicted probabilites
predict_probs <- predict(m0, type = "response")
df3m$predicted_probs <- predict_probs

# Aggregate predicted probabilities by species
avg_probs <- aggregate(predicted_probs ~ species, data = df3m, mean)

# Plotting
ggplot(avg_probs, aes(x = reorder(species, -predicted_probs), y = predicted_probs)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Predicted Probability of Being Observed Above 3 Meters by Species",
       x = "Species", y = "Predicted Probability") +
  theme_minimal()



# Adding Random effect ----------------------------------------------------


m1<- glmer(above3 ~ species + (1|tag_id), family="binomial", data = df3m, glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
summary(m0)
exp(coef(m0))
plot(m0)

# visualizing fixed effects 
library(ggplot2)

coef_df <- as.data.frame(confint(m0, method = "Wald"))  # Extracting confidence intervals
coef_df$species <- rownames(coef_df)
coef_df$OddsRatio <- exp(coef(m1))  # Convert log-odds to odds ratios

ggplot(coef_df, aes(x = species, y = OddsRatio)) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(2.5 %), ymax = exp(97.5 %)), width = 0.2) +
  coord_trans(y = "log") +  # Log scale for odds ratios
  theme_minimal() +
  labs(y = "Odds Ratio (with 95% CI)", x = "Shark Species", title = "Effect of Shark Species on Being Above 3 Meters")

# visualizing random effects
ranef_df <- as.data.frame(ranef(model)$SharkID)
ggplot(ranef_df, aes(x = condval)) +  # 'condval' are the conditional values of the random effects
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black") +
  theme_minimal() +
  labs(x = "Random Effect (SharkID)", y = "Frequency", title = "Distribution of Individual Shark Variability")






