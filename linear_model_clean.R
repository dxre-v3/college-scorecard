# Load in the Data
source(file = "scripts/sample.R")
# -- Linear - Models -- #
# consider the best models from the forests 
colleges_train %>%
  select("faminc", "compl_rpy_5yr_rt", "dep_inc_avg", 
         "pctpell", "md_faminc", "pct25_earn_wne_p6",
         "grad_debt_mdn", "ugds_black", "mn_earn_wne_indep0_p6", 
         "dep_inc_pct_lo", "pct10_earn_wne_p6") %>% skimr::skim()

# Create a training set 
college_vfold <- colleges_train %>%
  select("faminc", "compl_rpy_5yr_rt", "dep_inc_avg", "pctpell", "md_faminc", "pct25_earn_wne_p6",
              "grad_debt_mdn", "ugds_black", "mn_earn_wne_indep0_p6", 
              "dep_inc_pct_lo", "pct10_earn_wne_p6") %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))


# create a data set that fits a linear model to the folded data above
college_vfold %>% 
  mutate( 
    linear = map(.x = train, .f = function(x) glm(formula = compl_rpy_5yr_rt ~ .,
                                                 data = x, family = "binomial")),
    pred = map2(linear, test, predict),            
    test_mse = map2(test, pred, ~ (.x$compl_rpy_5yr_rt - .y)^2),
    mse = map_dbl(test_mse, na.rm = TRUE, mean),
    var = map_dbl(test, ~var(.x$compl_rpy_5yr_rt)),
    missingness = map_dbl(pred, ~ is.na(.x) %>% mean() )
    )   


# -------------------------------------------------------------------------

# That model didn't do so well, so lets try the other linear one 
# create second training set 
college_vfold_2 <- colleges_train %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))
# save formula
form <-  compl_rpy_5yr_rt ~ avgfacsal + pctpell + npt4_priv + sat_avg


college_vfold_2 %>% 
  mutate( 
    linear = map(.x = train, .f = function(x) glm(formula = compl_rpy_5yr_rt ~ avgfacsal + pctpell + npt4_priv + sat_avg,
                                                 data = x, family = "binomial")),
    pred = map2(linear, test, predict),          
    test_mse = map2(test, pred, ~ (.x$compl_rpy_5yr_rt - .y)^2),
    mse = map_dbl(test_mse, mean, na.rm = TRUE),
    var = map_dbl(test, ~var(.x$compl_rpy_5yr_rt)),
    missingness = map_dbl(pred, ~ is.na(.x) %>% mean() )
    )   

# -------------------------------------------------------------------------

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

# One more linear model then a lasso or ridge

college_vfold_3 <- colleges_train %>%
  select(corred) %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))

college_vfold_3 %>% 
  mutate( 
    linear = map(.x = train, .f = function(x) glm(formula = compl_rpy_5yr_rt ~ .,
                                                 data = x, family = "binomial")),
    pred = map2(linear, test, predict),   
    test_mse = map2(test, pred, ~ (.x$compl_rpy_5yr_rt - .y)^2),
    mse = map_dbl(test_mse, na.rm = TRUE, mean),
    var = map_dbl(test, ~var(.x$compl_rpy_5yr_rt)),
    missingness = map_dbl(pred, ~ is.na(.x) %>% mean() )
    )   


# -------------------------------------------------------------------------

# All in all, it appears linear models do exremely poorly so we will not be 
# carrying them back with us.
