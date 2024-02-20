pacman::p_load(fixest)


reg <- lm(data = all_merged, 
          entry ~ Count + KCI)

summary(reg)

feols_model <- feols(entry ~ Count + KCI + TCI | Region + period + Industry, data = all_merged)


summary(feols_model)
