# Load necessary libraries
library(fixest)
library(stargazer)

# Prepare your data
# Adjust this to your actual data source

# Define model formulas for different categories
model_formulas <- list(
  FULL = c("entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | City + period + Industry",
           "entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | period + Industry",
           "entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | City + Industry",
           "entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | period",
           "entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | City",
           "entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | Industry"),
  BASIC = c("entry ~ Count | City + period + Industry",
            "entry ~ Count | period + Industry",
            "entry ~ Count | City + Industry",
            "entry ~ Count | period",
            "entry ~ Count | City",
            "entry ~ Count | Industry"),
  COMPLEXITY = c("entry ~ Count + nKCI + nTCI + rca | City + period + Industry",
                 "entry ~ Count + nKCI + nTCI + rca | period + Industry",
                 "entry ~ Count + nKCI + nTCI + rca | City + Industry",
                 "entry ~ Count + nKCI + nTCI + rca | period",
                 "entry ~ Count + nKCI + nTCI + rca | City",
                 "entry ~ Count + nKCI + nTCI + rca | Industry"),
  CONTROL = c("entry ~ n_pub + share_students + population + gdp_c + pps | City + period + Industry",
              "entry ~ n_pub + share_students + population + gdp_c + pps | period + Industry",
              "entry ~ n_pub + share_students + population + gdp_c + pps | City + Industry",
              "entry ~ n_pub + share_students + population + gdp_c + pps | period",
              "entry ~ n_pub + share_students + population + gdp_c + pps | City",
              "entry ~ n_pub + share_students + population + gdp_c + pps | Industry")
)

# Automate model fitting
models <- list()
for (category in names(model_formulas)) {
  for (formula in model_formulas[[category]]) {
    model <- feols(as.formula(formula), data = data_normalized)
    models[[paste(category, formula, sep = ": ")]] <- model
  }
}

tmp <- model_formulas[['FULL']] %>% 
  map(function(x){feols(as.formula(x), data = data_normalized)})

# Summarize and table results
stargazer(feols_model1, feols_model2,
          title = "Model Summaries")



## FULL MODELS
feols_model1 <- feols(entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | City + period + Industry, data = data_normalized)
feols_model2 <- feols(entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | period + Industry , data = data_normalized)
feols_model3 <- feols(entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | City + Industry , data = data_normalized)
feols_model4 <- feols(entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | period , data = data_normalized)
feols_model5 <- feols(entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | City , data = data_normalized)
feols_model6 <- feols(entry ~ Count + pKCI + pTCI + rca + n_pub + share_students + population + gdp_c + pps | Industry , data = data_normalized)


stargazer(feols_model1, feols_model2, feols_model3, feols_model4, feols_model5, feols_model6,
          title = "Summary of FULL MODELS",
          type = "html") # Remove or change file name to directly view output in R console


# Use modelsummary to create a summary table
models <- list(feols_model1, feols_model2, feols_model3, feols_model4, feols_model5, feols_model6)
tmp <- modelsummary::modelsummary(models, output = "html", ) # or use `viewer = TRUE` to display in RStudio Viewer
