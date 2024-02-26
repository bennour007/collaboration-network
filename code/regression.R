pacman::p_load(fixest)



data_normalized <- data_full %>% 
  ungroup() %>% 
  group_by(period) %>% 
  mutate(
    sdKCI = sd(KCI), 
    mKCI = mean(KCI), 
    sdTCI = sd(TCI), 
    mTCI = mean(TCI),
    minKCI = min(KCI),
    maxKCI = max(KCI),
    minTCI = min(TCI),
    maxTCI = max(TCI)
  )  %>% 
  ungroup() %>% 
  group_by(City, period) %>% 
  mutate(
    nKCI = (KCI-mKCI)/sdKCI,
    pKCI = (KCI - minKCI)/(maxKCI - minKCI)
  ) %>% 
  ungroup() %>% 
  group_by(Industry, period) %>% 
  mutate(
    nTCI = (TCI-mTCI)/sdTCI,
    pTCI = (TCI - minTCI)/(maxTCI - minTCI)
  ) %>% 
  ungroup() 

  

  
data_normalized %>% 
  mutate(rca_binary = if_else(rca > .5, 1, 0)) %>%
  # filter(Region == 'University of Pecs') %>%
  filter(Region == 'University of Pecs') %>%
  filter((period == 2014 & rca_binary == 0) | (period == 2023 & rca_binary == 1)) %>%
  # arrange(desc(rca)) %>% 
  # slice_max(order_by = rca, n = 10) %>% 
  ggplot() +
  geom_point(
    aes(
      x = Count,
      y = pTCI,
      color = Industry,
      size = n_pub
    )
  ) +
  theme(legend.position="none") +
  facet_grid(period ~ entry )
  # geom_point(
  #   aes(
  #     x = period,
  #     y = n_pub
  #   )
  # )

  
  

  

## FULL MODELS
feols_model1 <- feols(entry ~ Count + pKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | City + period + Industry, data = data_normalized)
feols_model2 <- feols(entry ~ Count + pKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | period + Industry , data = data_normalized)
feols_model3 <- feols(entry ~ Count + pKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | City + Industry , data = data_normalized)
feols_model4 <- feols(entry ~ Count + pKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | period , data = data_normalized)
feols_model5 <- feols(entry ~ Count + pKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | City , data = data_normalized)
feols_model6 <- feols(entry ~ Count + pKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | Industry , data = data_normalized)


## BASIC MODELS
feols_model1 <- feols(entry ~ Count  | City + period + Industry, data = data_normalized)
feols_model2 <- feols(entry ~ Count  | period + Industry , data = data_normalized)
feols_model3 <- feols(entry ~ Count  | City + Industry , data = data_normalized)
feols_model4 <- feols(entry ~ Count  | period , data = data_normalized)
feols_model5 <- feols(entry ~ Count  | City , data = data_normalized)
feols_model6 <- feols(entry ~ Count  | Industry , data = data_normalized)

## COMPLEXITY MODELS
feols_model1 <- feols(entry ~ Count + nKCI + nTCI + rca  | City + period + Industry, data = data_normalized)
feols_model2 <- feols(entry ~ Count + nKCI + nTCI + rca  | period + Industry , data = data_normalized)
feols_model3 <- feols(entry ~ Count + nKCI + nTCI + rca  | City + Industry , data = data_normalized)
feols_model4 <- feols(entry ~ Count + nKCI + nTCI + rca  | period , data = data_normalized)
feols_model5 <- feols(entry ~ Count + nKCI + nTCI + rca  | City , data = data_normalized)
feols_model6 <- feols(entry ~ Count + nKCI + nTCI + rca  | Industry , data = data_normalized)

## CONTROL MODELS
feols_model1 <- feols(entry ~  n_pub + share_students + population + gdp_c + pps | City + period + Industry, data = data_normalized)
feols_model2 <- feols(entry ~  n_pub + share_students + population + gdp_c + pps | period + Industry , data = data_normalized)
feols_model3 <- feols(entry ~  n_pub + share_students + population + gdp_c + pps | City + Industry , data = data_normalized)
feols_model4 <- feols(entry ~  n_pub + share_students + population + gdp_c + pps | period , data = data_normalized)
feols_model5 <- feols(entry ~  n_pub + share_students + population + gdp_c + pps | City , data = data_normalized)
feols_model6 <- feols(entry ~  n_pub + share_students + population + gdp_c + pps | Industry , data = data_normalized)



summary(feols_model1)
summary(feols_model2)
summary(feols_model3)
summary(feols_model4)
summary(feols_model5)
summary(feols_model6)






reg <- lm(data = data_normalized, 
          entry ~ Count + nKCI + nTCI + rca + share_students + population + gdp_c + pps)

reg <- glm(data = data_normalized, 
           entry ~ Count + nKCI + nTCI + rca + share_students + population + gdp_c + pps,
           family = "binomial")

summary(reg)




# Calculate quartiles of the 'Count' column
quartiles <- quantile(data_normalized$Count, probs = c(0, 0.25, 0.5, 0.75, 1))

# Assign each row to a quartile group
data_normalized$Quartile <- cut(data_normalized$Count, breaks = quartiles, include.lowest = TRUE, labels = FALSE)

# Split the data by quartile group
data_split <- split(data_normalized, data_normalized$Quartile)

summa <- function(df) {
  # Perform any operation on df if needed, or simply return it
  ## FULL MODELS
  feols_model1 <- feols(entry ~ Count + nKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | City + period + Industry, data = df)
  feols_model2 <- feols(entry ~ Count + nKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | period + Industry , data = df)
  feols_model3 <- feols(entry ~ Count + nKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | City + Industry , data = df)
  feols_model4 <- feols(entry ~ Count + nKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | period , data = df)
  feols_model5 <- feols(entry ~ Count + nKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | City , data = df)
  feols_model6 <- feols(entry ~ Count + nKCI + nTCI + rca + n_pub + share_students + population + gdp_c + pps | Industry , data = df)
  
  l <- list(
    all_fe = summary(feols_model1),
    t_i_fe = summary(feols_model2),
    r_i_fe = summary(feols_model3),
    t_fe = summary(feols_model4),
    r_fe = summary(feols_model5),
    i_fe = summary(feols_model6)
  )
  return(l)
}

# Use map to create a list of data frames (or perform operations on each)
list_of_dfs <- map(data_split, summa)
