library(tidyverse)
library(MCMCglmm)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(broom)
library(car)
library(rstatix)
library(lme4)
library(tidyr)
library(DHARMa)
library(ggResidpanel)

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
    SD = sd(press)) %>% 
  mutate(thresher = ifelse(species == "Thresher", 1,0))


all <-ggplot() +
  geom_boxplot(data=filter(agg_df, thresher == 0),
               aes(x=species,y=median_press),outlier.shape = NA) +
  geom_label(data=filter(stats_df, thresher == 0),
            aes(label = paste(round(MedianDepth,2)),
                x = species, y = MedianDepth), vjust = 0.5, size =3,
            fill = "white") +
  geom_text(data=filter(stats_df, thresher == 0),
            aes(label = paste("\nN:", Count),
                x = species, y = 30), vjust = 2.7, size =3) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  scale_x_discrete(labels = function(x) str_replace_all(x, " ", "\n")) +
  scale_y_reverse(limits = c(30, 0)) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(t = 10, r = 5, b = 50, l = 5, unit = "pt"),
        axis.title.x = element_text(margin = margin(t = 25, b = -20)),
        axis.text.x = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),) +
  coord_cartesian(clip="off") +
  labs(title = "Distribution of Median Depth by Species", x = "Species", y = "Median Depth")

thresher <- ggplot() +
  geom_boxplot(data=filter(agg_df, thresher == 1),
               aes(x=species,y=median_press),outlier.shape = NA) +
  geom_label(data=filter(stats_df, thresher == 1),
             aes(label = paste(round(MedianDepth,2)),
                 x = species, y = MedianDepth), vjust = 0.5, size =3,
             fill = "white") +
  geom_text(data=filter(stats_df, thresher == 1),
            aes(label = paste("\nN:", Count),
                x = species, y = 140), vjust = 2, size =3) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  scale_x_discrete(labels = function(x) str_replace_all(x, " ", "\n")) +
  scale_y_reverse(limits = c(140, 0)) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(t = 25, r = 25, b = 50, l = 25, unit = "pt"),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        plot.title = element_blank(),
        axis.text.x = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()) +
  coord_cartesian(clip="off") +
  labs(title = "Distribution of Median Depth by Species", x = "Species", y = "Median Depth")

library(cowplot)
x <- plot_grid(all, thresher, ncol = 2, rel_widths = c(2, 1))


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
write.csv(df3m, "df3m.csv")

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



# Time above 1m -----------------------------------------------------------

df1m <- dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  mutate(above1 = ifelse(press<=1,1,0)) %>% 
  group_by(tag_id) %>% 
  mutate(count = n()) %>% ungroup() %>% 
  mutate(species = as.factor(species))

df1mI <- dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  mutate(above1 = ifelse(press<=1,1,0))  %>% 
  group_by(tag_id) %>% 
  summarize(
    total_count = n(),
    above1_count = sum(above1),
    percent_above1 = (above1_count / total_count) 
  ) #%>% filter(total_count > 100)

df1mS <-dets_pa_est_tod %>% 
  filter(tod == "Day") %>% 
  mutate(above1 = ifelse(press<=1,1,0))  %>% 
  group_by(species) %>% 
  summarize(
    total_count = n(),
    above1_count = sum(above1),
    percent_above1 = (above1_count / total_count) 
  ) #%>% filter(total_count > 100)


ggplot(df1mS,
       aes(x=reorder(species, percent_above1), y=percent_above1, fill=species)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Proportion of Time Spent in Top 1 Meters by Shark Species", x="Species", y="Proportion") +
  coord_flip() 

ggplot(df1mI,
       aes(x=reorder(tag_id, percent_above1), y=percent_above1)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Proportion of Time Spent in Top 1 Meters by Shark Species", x="Species", y="Proportion") +
  coord_flip() 



# dumbell 3 vs 1 ----------------------------------------------------------

df_diff <- left_join(df3mS, df1mS, by = "species") %>%
  select(species, percent_above3, percent_above1) %>% 
  mutate(diff = percent_above3 - percent_above1) %>% 
  pivot_longer(cols = c(percent_above3, percent_above1), names_to = "depth", values_to = "percent") 
  

stats <- df_diff %>%
  group_by(depth) %>%
  summarise(mean = mean(percent),
            SE = sd(percent)) %>%
  mutate(meanpos = mean + 1 *SE,
         meanneg = mean - 1 *SE)

stats_3 <- stats %>%
  filter(depth == "percent_above3")

stats_1 <- stats %>%
  filter(depth == "percent_above1")

diff <- df_diff %>% 
  filter(depth == "percent_above1") %>% 
  mutate(x_pos = percent + (diff/2)) 

ggplot(df_diff) +
  geom_segment(data = df3mS,
               aes(x = percent_above3, y = reorder(species, percent_above3),
                   yend = df1mS$species, xend = df1mS$percent_above1),
              color = "#aeb6bf",
              size = 4.5,
              alpha = .5) +
  geom_point(aes(x = percent, y = species, color = depth), size = 4) +
  geom_text(data = filter(diff, diff > 0.08),
            aes(label = paste("D: ",round(diff,3)), x = x_pos, y = species), 
            color = "#4a4e4d",
            size = 2.5) +
  geom_vline(xintercept = stats_1$mean, linetype = "solid", size = .5, alpha = .8, color = "#009688") +
  geom_vline(xintercept = stats_3$mean, color = "#762a83", linetype = "solid",  size = .5, alpha = .8) +
  scale_color_manual(values = c("#009688","#762a83")) +
  geom_text(x = stats_1$mean - 0.018 , y = 9.2, label = "MEAN", angle = 90, size = 2.5, color = "#009688", check_overlap = TRUE) +
  geom_text(x = stats_3$mean - 0.018 , y = 9.2, label = "MEAN", angle = 90, size = 2.5, color = "#762a83", check_overlap = TRUE)

# glmre -------------------------------------------------------------------
#df3m <- read_csv("/df3m.csv")
small <- c("Thresher", "Blacktip", "Tiger", "Smooth Hammerhead")
# excluding these we get 1416435 rows
small_df <- df3m %>% 
  filter(! species %in% small) %>% 
  sample_n(20000)  %>% 
  mutate(species = as.factor(species),
         tag_id = as.factor(tag_id))


# glmer -------------------------------------------------------------------

m1 <- glmer(above3 ~ species + (1|tag_id) -1,
            family="binomial", data = df3m,
            glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) #can add nAGQ = 0, sacrifices accuracy for speed in processing
summary(m1)
printCoefmat(coef(summary(m1)),digits=2)
resid_panel(m1)
simulationOutput <- simulateResiduals(fittedModel = m1, plot = F)
plot(simulationOutput)

m2<- glmer(above3 ~ species + (1|tag_id) -1, 
           family=binomial(link = "logit"), data = df3m, 
           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m2)
printCoefmat(coef(summary(m2)),digits=2)
simulationOutput <- simulateResiduals(fittedModel = m2)
plot(simulationOutput)

m3 <- glmer(above3 ~ species + (1|tag_id) -1,
            family="binomial", data = df3m,
            glmerControl(optimizer = "optimx", calc.derivs = FALSE, 
                         optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) #more speed, sacrifices accuracy
summary(m3)

m4<- glmer(above3 ~ species + (1|tag_id) -1, 
           family=binomial(link = "logit"), data = df3m, nAGQ=0, 
           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m4)

anova(m2, m4)

  #some other things to try            
      verbose=TRUE, nAGQ=0, control=glmerControl(optimizer = "nloptwrap"
                     
# Assuming m1 is your glmer model
# Expand new_data as before if not already defined
new_data <- with(df3m, expand.grid(species = unique(species)))

# Predict log odds
new_data$log_odds <- predict(m1, newdata = new_data, re.form = NA, type = "link")
new_data$probability = predict(m1, newdata = new_data, re.form = NA, type = "response")


# Calculate standard errors for predictions
se_log_odds <- predict(m1, newdata = new_data, re.form = NA, type = "link", se.fit = TRUE)

# Calculate confidence intervals for log odds
alpha <- 0.05 # For 95% CI
z <- qnorm(1 - alpha / 2)
new_data$log_odds_lower <- new_data$log_odds - z * se_log_odds
new_data$log_odds_upper <- new_data$log_odds + z * se_log_odds

# Transform log odds CI to probability scale
new_data$probability_lower <- plogis(new_data$log_odds_lower)
new_data$probability_upper <- plogis(new_data$log_odds_upper)

ggplot(new_data, aes(x = species)) +
  geom_point(aes(y = log_odds)) +
  geom_errorbar(aes(ymin = log_odds_lower, ymax = log_odds_upper), width = 0.2) +
  theme_minimal() +
  labs(title = "Log Odds for Fixed Covariates (Species) with Confidence Intervals",
       x = "Species", y = "Log Odds")

ggplot(new_data, aes(x = species)) +
  geom_point(aes(y = probability)) +
  geom_errorbar(aes(ymin = probability_lower, ymax = probability_upper), width = 0.2) +
  theme_minimal() +
  labs(title = "Predicted Probabilities for Fixed Covariates (Species) with Confidence Intervals",
       x = "Species", y = "Probability")



# mcmcm -------------------------------------------------------------------

# 
 m3 <- MCMCglmm(above3~species - 1,
                random=~tag_id,data=df3m,
                family="categorical",
                verbose=FALSE)

 m1 <- MCMCglmm(above1~species - 1,
                random=~tag_id,data=df1m,
                family="categorical",
                verbose=FALSE)

summary(m3)
summary(m1)

par(mfrow=c(9,2))
plot(m6$Sol, auto.layout=T)


posterior_means3 <- m3$Sol
posterior_means1 <- m1$Sol


posterior_long3 <- posterior_means3 %>% 
  as.data.frame() %>%
  rownames_to_column("Parameter") %>%
  pivot_longer(-Parameter, names_to = "Iteration", values_to = "Value") #%>% 
#mutate(Value = exp(Value))

posterior_long1 <- posterior_means1 %>% 
  as.data.frame() %>%
  rownames_to_column("Parameter") %>%
  pivot_longer(-Parameter, names_to = "Iteration", values_to = "Value") #%>% 
#mutate(Value = exp(Value))


ggplot(posterior_long3, aes(y = Iteration, x = Value)) +
  stat_halfeye() +
  theme_minimal() +
  labs(title = "Posterior Distributions with Credible Intervals",
       x = "Value",
       y = "Parameter")

ggplot(posterior_long1, aes(y = Iteration, x = Value)) +
  stat_halfeye() +
  theme_minimal() +
  labs(title = "Posterior Distributions with Credible Intervals",
       x = "Value",
       y = "Parameter")

species_effects_df3 <- data.frame(posterior_means3) %>%
  pivot_longer(cols = everything(), names_to = "species", values_to = "posterior_draw") %>% 
  mutate(p = exp(posterior_draw)/(1+exp(posterior_draw))) %>% 
  group_by(species) %>% 
  summarise(p_mean = mean(p),
            cilo = quantile(p, probs = 0.025),
            cihi = quantile(p, probs = 0.975))

species_effects_df1 <- data.frame(posterior_means1) %>%
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


ggplot(species_effects_df1, aes(x = species, y = p_mean)) +
  geom_point(fill = "skyblue") +
  geom_errorbar(aes(ymin = cilo, ymax = cihi), width = 0.2) +
  labs(title = "Predicted Probability of 'Above1' by Species",
       x = "Species",
       y = "Predicted Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))