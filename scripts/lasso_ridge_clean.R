# load data
source(file = "scripts/sample.R")
# -- Lasso - Model - Set -- #
library(glmnet) # ridge & lasso
library(glmnetUtils) # improves working with glmnet

# Make corr plot so we can see what the most linear are 
cor_data <- colleges_train %>% 
  select_if(is.numeric) %>% 
  cor() %>% as.data.frame()

# get correlation data 
corred <- cor_data["compl_rpy_5yr_rt", ] %>% 
  tibble() %>% 
  pivot_longer(c(-unitid, -latitude, -longitude), names_to = "name", values_to = "val") %>% 
  filter(!is.na(val)) %>% 
  arrange(desc(val)) %>% 
  pluck(4)

# -------------------------------------------------------------------------
# avgfacsal, pctpell, avgfacsal npt4_pub npt4_priv sat_avg


college_vfold <- colleges_train %>%
  select(corred) %>%
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))




# form <-  compl_rpy_5yr_rt ~ avgfacsal + pctpell + sat_avg

# -------------------------------------------------------------------------


colleges_train %>% 
  glmnetUtils::cv.glmnet(
    formula = compl_rpy_5yr_rt ~ avgfacsal + pctpell + npt4_pub + sat_avg,
    data = ., 
    alpha = 0, 
    nfolds = 5
    )


# -------------------------------------------------------------------------

best_forest <- college_vfold %>% pluck(3,1) %>% 
  glmnetUtils::cv.glmnet(
    formula = compl_rpy_5yr_rt ~ .,
    data = ., 
    alpha = 0, 
    nfolds = 10
    )
plot(best_forest)

r_min <- best_forest$lambda.min
r_1se <- rbest_forest$lambda.1se

college_vfold %>% 
  mutate(
    ridge = map(train, ~ glmnetUtils::cv.glmnet(
      formula = compl_rpy_5yr_rt ~ .,
      data = .x, 
      alpha = 0, 
      nfolds = 5
    ))
  )
# Ridge not working bc is constant?? 




ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

colleges_train$compl_rpy_5yr_rt %>% skimr::skim()
