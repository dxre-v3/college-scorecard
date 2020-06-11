# load data

source(file = "scripts/sample.R")
# -- Lasso - Model - Set -- #


avgfacsal, pctpell, avgfacsal npt4_pub npt4_priv sat_avg


college_vfold <- colleges_train %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))


form <-  compl_rpy_5yr_rt ~ avgfacsal + pctpell + npt4_priv + sat_avg

lm(form, colleges_train, na.action="na.exclude")




college_vfold %>% 
  mutate( 
    linear = map(.x = train, .f = function(x) lm(formula = form, data = x)),
    pred = map2(linear, test, predict),            
    test_mse = map2_dbl(test, pred, ~ mean((.x$compl_rpy_5yr_rt - .y)^2))
    ) %>% pluck("pred", 1)

# So the rate of missingnesss is getting really really frustrating. 

colleges_train %>% select(pctpell, avgfacsal,npt4_pub,  npt4_priv ,sat_avg) %>% skimr::skim()

c(F, T, T) %>% mean()

form <-  compl_rpy_5yr_rt ~ avgfacsal + pctpell + sat_avg
linearness <- college_vfold %>% 
  mutate( 
    linear = map(.x = train, .f = function(x) lm(formula = form, data = x, na.action = "na.exclude")),
    pred = map2(linear, test, predict),            
    test_mse = map2(test, pred, function(x, y) (x$compl_rpy_5yr_rt - y)^2),
    num_na = map(pred, function(x) x %>% is.na),
    num_na = map_dbl(num_na, mean), 
    test_mse = map_dbl(test_mse, na.rm=TRUE, mean),
    )

linearness %>% 
  summarise(
    na = mean(num_na),
    accuracy = mean(test_mse)
  )
# So even though the linear model is extremely accurate, it only has about about 74 percent 
# missingness. Unless, I can fix that, this particular model is useless. 


library(glmnet) # ridge & lasso
library(glmnetUtils) # improves working with glmnet

colleges_train %>% 
  glmnetUtils::cv.glmnet(
    formula = compl_rpy_5yr_rt ~ avgfacsal + pctpell + avgfacsal + npt4_pub + npt4_priv + sat_avg,
    data = ., 
    alpha = 0, 
    nfolds = 5
    )
# Ridge not working bc is constant?? 




ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

colleges_train$compl_rpy_5yr_rt %>% skimr::skim()
