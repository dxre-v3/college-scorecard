# FINAL MODEL SELECTION - - - Correlated Data ---------------------------------------
# Explanation:

# The final models I selected to compare were the lasso, ridge and forests.
# While I also tried using Support Vector Machines and Linear Models, 
# both did not do particularly well, linear models especially adding in a to more 
# noise. 

# Data Set Up -------------------------------------------------------------

# Load the data, library and seed
source(file = "scripts/sample.R")

# -- Specific - Libraries - Set -- #
library(xgboost) # forests
library(glmnet) # ridge & lasso
library(glmnetUtils) # better glmnet


# -------------------------------------------------------------------------
# Load Models. 
# -------------------------------------------------------------------------

# Load the lassos and ridges --------------

lr <- read_rds('data/processed/lasso_ridge.rds')
# add the data for later 

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

lr <- lr %>% 
  mutate(
    select_on = colleges_select %>% select(all_of(selection)) %>% list()
  )

# Load the forests  --------------------
forests <- readr::read_rds('data/processed/college_forests.rds')

# helper matrix function
xgb_matrix <- function(dat, outcome){
    pred <- dat[[outcome]]
    mat <- dat %>% dplyr::select(-outcome) %>% # encode on full boston df
      onehot::onehot() %>% # use onehot to encode variables
      predict(dat) # get OHE matrix
    return(xgb.DMatrix(data = mat, 
                       label = pred))
}

# Save selection data onto the dataset 
forests <- forests %>% select(-dat) %>% 
  mutate(select_on = xgb_matrix(colleges_select %>%
                            select(- unitid, - instnm, - city, - latitude, - longitude, -stabbr),
                          "compl_rpy_5yr_rt") %>% list()
         )
  

# Final Model Selection  --------------------------------------------------

# Forest error function
xg_error <- function(model, test_mat, metric = "mse"){
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  
  if(metric == "mse"){
    err <- mean((preds - vals)^2)
  } else if(metric == "misclass") {
    err <- mean(preds != vals)
  }
  
  return(err)
}

# Ridge Lasso Error function

lr_error <- function(data, model){
  true_vals <- data$compl_rpy_5yr_rt
  # preds <- predict(model, newdata = data %>% as_tibble())
  # test_mse <- mean((true_vals - preds)^2)
  data %>%
    mutate(pred_prob = predict(model, newdata = data),
           test_mse = map_dbl(pred_prob, ~ mean((true_vals - .x)^2))
           )%>%
    pull(test_mse) %>% 
    mean()
  # return(test_mse)
}

# pred = map2(fit, test, predict),
         # test_mse = map2_dbl(test, pred, ~ mean((.x$compl_rpy_5yr_rt - .y)^2)))


# -------------------------------------------------------------------------
# Get MSE/Error Rate
forests %>% 
  mutate(
   mse = map2_dbl(xg_model, select_on, xg_error, metric = "mse"),
   var = var(colleges_select$compl_rpy_5yr_rt),
   error_rate = mse/var
  ) %>% 
  select(-xg_model, -select_on)

# A tibble: 4 x 5
#       nums title             mse    var error_rate
#      <dbl> <chr>           <dbl>  <dbl>      <dbl>
# 1  0.00197 boosted_1     0.0420  0.0181      2.33 
# 2  0.00654 boosted_2     0.0100  0.0181      0.555
# 3  0.0721  boosted_3     0.00305 0.0181      0.169
# 4 NA       random_forest 0.00297 0.0181      0.165

lr %>% 
  mutate(
   pred = map2(models, select_on, predict),          
    test_mse = map2(select_on, pred, ~ (.x$compl_rpy_5yr_rt - .y)^2),
    mse = map_dbl(test_mse, mean, na.rm = TRUE),
    var = map_dbl(select_on, ~var(.x$compl_rpy_5yr_rt)),
    percent = mse/var) %>% 
  select(-models, -select_on, -pred, -test_mse)


# A tibble: 4 x 4
#   title         mse    var percent
#   <chr>       <dbl>  <dbl>   <dbl>
# 1 ridge_min 0.00413 0.0181   0.229
# 2 ridge_lse 0.00420 0.0181   0.232
# 3 lasso_min 0.00424 0.0181   0.235
# 4 lasso_lse 0.00448 0.0181   0.248

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# PERFORMANCE FOR SELECTED MODELS 
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

forests %>% filter(title == "boosted_3" |title == "random_forest") %>% 
  select(-select_on) %>% 
    mutate(performance = xgb_matrix(colleges_perform %>%
                            select(- unitid, - instnm, - city, - latitude, - longitude, -stabbr),
                          "compl_rpy_5yr_rt") %>% list(),
   mse = map2_dbl(xg_model, performance, xg_error, metric = "mse"),
   var = var(colleges_perform$compl_rpy_5yr_rt),
   error_rate = mse/var
  ) %>% 
  select(-xg_model, -performance)

#      nums title             mse    var error_rate
#     <dbl> <chr>           <dbl>  <dbl>      <dbl>
# 1  0.0721 boosted_3     0.00303 0.0157      0.192
# 2 NA      random_forest 0.00252 0.0157      0.160

# So at the end of the day, the best models are random forests :)

# -------------------------------------------------------------------------
# A GRAPH
# -------------------------------------------------------------------------
xg_pred <- function(model, test_mat, metric = "mse"){
  preds = predict(model, test_mat)
  return(preds%>% unlist())
}


y <- forests %>% filter(title == "boosted_3" |title == "random_forest") %>% 
  select(-select_on) %>% 
    mutate(performance = xgb_matrix(colleges_perform %>%
                            select(- unitid, - instnm, - city, - latitude, - longitude, -stabbr),
                          "compl_rpy_5yr_rt") %>% list(),
   mse = map2(xg_model, performance, xg_pred, metric = "mse")) %>% 
  pluck(5,2)

fgraph <- colleges_perform %>% mutate(pred = y) %>% 
  ggplot(aes(compl_rpy_5yr_rt, pred))+
  geom_point(color = '#269844', alpha = 0.5) +
    labs(
      y = 'Predictions',
      x = "Five-year repayment rate for completers"
    ) +
    theme_minimal()


fg <- colleges_perform %>% mutate(pred = y) %>% select(pred, compl_rpy_5yr_rt)

# write.csv(fg, 'documents/graphic.csv')
