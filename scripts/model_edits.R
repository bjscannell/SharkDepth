
sample_df3 <- df3m %>% 
  group_by(species) %>% 
  sample_n(500) %>% 
  mutate(hour = hour(date_times_est))

set.seed(32)

m3 <- glmer(above3 ~ species + hour + (1|tag_id) - 1,
            family="binomial", data = sample_df3,
            glmerControl(optimizer = "optimx", calc.derivs = FALSE, 
                         optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) #more speed, sacrifices accuracy

