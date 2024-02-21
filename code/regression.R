pacman::p_load(fixest)


reg <- lm(data = all_merged, 
          entry ~ Count + KCI)

reg <- glm(data = allmerged_region1, 
          entry ~ Count + nKCI,
          family = "binomial")

summary(reg)

hist(all_merged$KCI)

allmerged_region1 <- all_merged_regions %>% 
  ungroup() %>% 
  group_by(period) %>% 
  mutate(sdKCI = sd(KCI), mKCI = mean(KCI), sdTCI = sd(TCI), mTCI = mean(TCI))  %>% 
  ungroup() %>% 
  group_by(Region, period) %>% 
  mutate(nKCI = (KCI-mKCI)/sdKCI) %>% 
  ungroup() %>% 
  group_by(Industry, period) %>% 
  mutate(nTCI = (TCI-mTCI)/sdTCI)
  
ggplot(allmerged_region1) + 
  geom_histogram(aes(x = (nKCI))) +
  facet_grid(~ entry)
  
  
  all_merged %>% 
  ggplot(aes(x = Count)) + 
  geom_histogram() + facet_grid(~ entry, scales = 'free_x')
  


feols_model <- feols(entry ~ Count + nKCI + nTCI | Region + period + Industry, data = allmerged_region1)
feols_model <- feols(entry ~ Count + nKCI + nTCI | period + Industry , data = allmerged_region1)
feols_model <- feols(entry ~ Count + nKCI + nTCI | Region + Industry , data = allmerged_region1)
feols_model <- feols(entry ~ Count + nKCI + nTCI | period , data = allmerged_region1)
feols_model <- feols(entry ~ Count + nKCI + nTCI | Region , data = allmerged_region1)
feols_model <- feols(entry ~ Count + nKCI + nTCI | period + Industry , data = allmerged_region1)
summary(feols_model)


reg <- lm(data = allmerged_region1, 
          entry ~ Count + nKCI + nTCI)

reg <- glm(data = allmerged_region1, 
           entry ~ Count + nKCI + nTCI,
           family = "binomial")

summary(reg)
