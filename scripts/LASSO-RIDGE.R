# LASSO/RIDGE - - - Correlated Data ---------------------------------------
# Explanation:

# This file is run to see how a lasso and a ridge model fit when only
# correlated values are kept. To do this, I loaded the data, then saved 
# the names of correlated variables under `keep`. I ran both a lasso and 
# a ridge with the formula response by remaining. 


# Data Set Up -------------------------------------------------------------

# Load the data, library and seed
source(file = "scripts/sample.R")
# -- Specific - Libraries - Set -- #
library(glmnet) # ridge & lasso
library(glmnetUtils) # better glmnet


# Select Variables --------------------------------------------------------

# + Selection Data
# Make corr plot so we can see what the most linear are 
selection_data <- colleges_train %>% 
  select_if(is.numeric) %>% 
  cor() %>% as.data.frame()

# + Selection of variables
# get correlation data 
selection <- selection_data["compl_rpy_5yr_rt", ] %>% 
  tibble() %>% 
  pivot_longer(c(-unitid, -latitude, -longitude), names_to = "name", values_to = "val") %>% 
  filter(!is.na(val)) %>% 
  arrange(desc(val)) %>% 
  pluck(4)


# -------------------------------------------------------------------------
# Training Data Set
# -------------------------------------------------------------------------


college_vfold <- colleges_train %>%
  select(all_of(selection)) %>%
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))

# -------------------------------------------------------------------------
# RIDGE MODEL
# -------------------------------------------------------------------------

# A crossfold training for lambda
ridge_test <- college_vfold %>% pluck(3,1) %>% 
  glmnetUtils::cv.glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 0, 
    nfolds = 10
    )

# Optional viewing of the lambda
plot(ridge_test)

# Save the best lambdas
r_min <- ridge_test$lambda.min
r_lse <- ridge_test$lambda.1se

# Fit the models on the crossfolded data set 
ridge_model <- college_vfold %>% 
  crossing(lambda = c(r_min, r_lse)) %>% 
  mutate(
    ridge = map2(train, lambda, ~ glmnetUtils::glmnet(
      formula = compl_rpy_5yr_rt ~ .,
      data = .x, 
      alpha = 0, 
      lambda = .y
    ))
  )

# Get the Test MSE, variance and percent error
ridge_model <- ridge_model %>% 
  mutate(
    pred = map2(ridge, test, predict),          
    test_mse = map2(test, pred, ~ (.x$compl_rpy_5yr_rt - .y)^2),
    mse = map_dbl(test_mse, mean, na.rm = TRUE),
    var = map_dbl(test, ~var(.x$compl_rpy_5yr_rt)),
    missingness = map_dbl(pred, ~ is.na(.x) %>% mean() ),
    percent = mse/var
    )

# Get the average error rate for both models 
ridge_model %>% group_by(lambda) %>% 
  summarise(error_rate = mean(percent))

#    lambda error_rate
#     <dbl>      <dbl>
# 1 0.00955      0.283
# 2 0.0423       0.294

# -------------------------------------------------------------------------
# LASSO MODEL
# -------------------------------------------------------------------------

# A crossfold training for lambda
lasso_test <- college_vfold %>% pluck(3,1) %>% 
  glmnetUtils::cv.glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 1, 
    nfolds = 10
    )

# Optional viewing of the lambda
plot(lasso_test)

# Save the best lambdas
L_min <- lasso_test$lambda.min
L_lse <- lasso_test$lambda.1se

# Fit the models on the crossfolded data set 
lasso_model <- college_vfold %>% 
  crossing(lambda = c(L_min, L_lse)) %>% 
  mutate(
    ridge = map2(train, lambda, ~ glmnetUtils::glmnet(
      formula = compl_rpy_5yr_rt ~ .,
      data = .x, 
      alpha = 1, 
      lambda = .y
    ))
  )

# Get the Test MSE, variance and percent error
lasso_model <- lasso_model %>% 
  mutate(
    pred = map2(ridge, test, predict),          
    test_mse = map2(test, pred, ~ (.x$compl_rpy_5yr_rt - .y)^2),
    mse = map_dbl(test_mse, mean, na.rm = TRUE),
    var = map_dbl(test, ~var(.x$compl_rpy_5yr_rt)),
    missingness = map_dbl(pred, ~ is.na(.x) %>% mean() ),
    percent = mse/var
    )

# Get the average error rate for both models 
lasso_model %>% group_by(lambda) %>% 
  summarise(error_rate = mean(percent))

#    lambda error_rate
#     <dbl>      <dbl>
# 1 0.00145      0.283
# 2 0.00643      0.306


# -------------------------------------------------------------------------
# SAVE FINAL MODELS
# -------------------------------------------------------------------------

forward_models <- colleges_train %>%
  select(all_of(selection))


# Ridge values ------------------------------------------------------------

ridge <- forward_models %>% 
  glmnetUtils::cv.glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 0, 
    nfolds = 10
    )

# Optional viewing of the lambda
# plot(ridge)

# Save the best lambdas
final_rmin <- ridge$lambda.min
final_rlse <- ridge$lambda.1se

# Fit Final Models 
ridge_min <- forward_models %>% 
  glmnetUtils::glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 0, 
    lambda = final_rmin
    )

ridge_lse <- forward_models %>% 
  glmnetUtils::glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 0, 
    lambda = final_rlse
    )

# Lasso values ------------------------------------------------------------

lasso <- forward_models %>% 
  glmnetUtils::cv.glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 1, 
    nfolds = 10
    )

# Optional viewing of the lambda
# plot(lasso)

# Save the best lambdas
final_Lmin <- lasso$lambda.min
final_Lse <- lasso$lambda.1se

# Fit Final Models 
lasso_min <- forward_models %>% 
  glmnetUtils::glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 1, 
    lambda = final_Lmin
    )

lasso_lse <- forward_models %>% 
  glmnetUtils::glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 1, 
    lambda = final_Lse
    )

# Save final models -------------------------------------------------------

lr <- tibble(
  title = c("ridge_min", "ridge_lse", "lasso_min", "lasso_lse"),
  models = c(ridge_min %>% list(), ridge_lse %>% list(), 
             lasso_min %>% list(), lasso_lse %>% list())
  
)

# write_rds(lr, 'data/processed/lasso_ridge.rds')