# load data
source(file = "scripts/sample.R")
# -- Forest - Model - Set -- #

# === Libraries === #
# library(ranger)
# library(vip)
# library(pdp)

library(xgboost)
## = Random Forests via Xgboost = ##
xgb_matrix <- function(dat, outcome){
    pred <- dat[[outcome]]
    mat <- dat %>% dplyr::select(-outcome) %>% # encode on full boston df
      onehot::onehot() %>% # use onehot to encode variables
      predict(dat) # get OHE matrix
    return(xgb.DMatrix(data = mat, 
                       label = pred))
}

xg_error <- function(model, test_mat, metric = "mse"){
  
  # Get predictions and actual values
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  
  if(metric == "mse"){
    
    # Compute MSE if that's what we need
    err <- mean((preds - vals)^2)
    
  } else if(metric == "misclass") {
    
    # Otherwise, get the misclass rate
    err <- mean(preds != vals)
    
  }
  
  return(err)
}


no_char_coll <- colleges_train %>% 
  select(- unitid, - instnm, - city, - latitude, - longitude, -stabbr)

exttra<- xgb_matrix(no_char_coll, "compl_rpy_5yr_rt" )

# 
xg_college <- no_char_coll %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing)) %>% 
   # crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>% 
  mutate(train_mat = map(train, xgb_matrix, outcome = "compl_rpy_5yr_rt"),
         test_mat = map(test, xgb_matrix, outcome = "compl_rpy_5yr_rt"))
#

xg_random_forest <- xg_college %>% 
  mutate(xg_model = map(.x = train_mat, 
                          .f = function(x) xgb.train(params = list(colsample_bynode = 0.8,
                                                                      learn_rate = 1, 
                                                                      max_depth = 5,
                                                                      num_parallel_tree = 100,
                                                                      objective = "reg:squarederror",
                                                                      subsample = 0.8
                                                                      ),
                                                     data = x,
                                                     nrounds = 200,
                                                     num_boost_round = 1
                                                        )
                        )
  )

(xg_random_forest <- xg_random_forest %>% 
  mutate(
    xg_train_mse = map2_dbl(xg_model, train_mat, xg_error, metric = "mse"),
    xg_test_mse = map2_dbl(xg_model, test_mat, xg_error, metric = "mse")
    ))

# So Using a form of random forests via xgboost on the data produced pretty good results 
# using the 110 varaiables. lets see if we can get the importance on top of that. 

rf_names <- xg_random_forest %>% pluck("train", 1) %>% colnames() %>% as.vector()
rf_m1 <- xg_random_forest %>% pluck("xg_model", 1)

xgb.importance(rf_names, model = rf_m1)%>% as.tibble() %>% 
  arrange(desc(Gain)) %>% 
  head(10) %>% 
  pluck(1)

xg_random_forest %>% pluck("xg_model", 1)
# Well. So i got importance but I dont understand it at all.... ok so i kinda understand it
# We're only going to use gain here. 

xg_topn = function(df, index, length = 5){
  names <- df %>% pluck("train", index) %>% colnames() %>% as.vector()
  model <- models %>% pluck("xg_model", index)
  
  xgb.importance(names, model = model)%>% as_tibble() %>% 
  arrange(desc(Gain))%>% 
  head(length) %>% 
  pluck(1)
}



# xg_topn(xg_random_forest, 1) === a test ===

xg_random_forest <- xg_random_forest %>% 
  mutate(num = dplyr::row_number(),
    top5_vars = map(.x = num, ~ xg_topn(xg_random_forest, index = .x, 10)))

# time to compare

xg_random_forest %>% unnest(top5_vars) %>% 
  count(top5_vars)

# We'll see if this remains consistent, but this looks interesting for sure.  

# -------------------------------------------------------------------------

xgb_college <- no_char_coll %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing)) %>% 
   crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>%
  mutate(train_mat = map(train, xgb_matrix, outcome = "compl_rpy_5yr_rt"),
         test_mat = map(test, xgb_matrix, outcome = "compl_rpy_5yr_rt"))

# boosted model 
xg_boost <- xgb_college %>% 
  mutate(xg_model = map2(.x = train_mat, .y = learn_rate, 
                          .f = function(x, y) xgb.train(params = list(eta = y,
                                                                      depth = 10,
                                                                      objective = "reg:squarederror"),
                                                     data = x,
                                                     nrounds = 200
                                                     )
                        )
  )
  

(xg_boost <- xg_boost %>% 
  mutate(
    xg_train_mse = map2_dbl(xg_model, train_mat, xg_error, metric = "mse"),
    xg_test_mse = map2_dbl(xg_model, test_mat, xg_error, metric = "mse")
    ))

xg_boost %>% 
  group_by(learn_rate) %>% 
  summarise(train_mse = mean(xg_train_mse),
            test_mse = mean(xg_test_mse)) %>% view



# well it looks like the best learning rate may be the 18th one. 
# However i don't want to over fit so I will also keep 15 and 16
# 1.971228e-03 <- 0.0434
# 6.543189e-03 <- .0104
# 7.209327e-02 <best with 0.00332

# -------------------------------------------------------------------------
# fitting another random forest model?

forest_matrix <- xgb_matrix(no_char_coll, "compl_rpy_5yr_rt")

random_forward <-  xgb.train(params = list(colsample_bynode = 0.8,
                                           learn_rate = 1, 
                                           max_depth = 5,
                                           num_parallel_tree = 100,
                                           objective = "reg:squarederror",
                                           subsample = 0.8
                                           ),
                             data = forest_matrix,
                             nrounds = 200,
                             num_boost_round = 1
                             )

 college_forests <- tibble(
    nums = c(1.971228e-03, 6.543189e-03, 7.209327e-02),
    dat = c(forest_matrix)
  ) %>% 
    mutate(xg_model = map2(.x = dat, .y = nums, 
                          .f = function(x, y) xgb.train(params = list(eta = y,
                                                                      depth = 10,
                                                                      objective = "reg:squarederror"),
                                                     data = x,
                                                     nrounds = 200
                                                     )
                        )
  )

college_forests <- college_forests %>%
  mutate(title = str_c("boosted_", row_number())) %>% 
  add_row(nums = NA, dat = forest_matrix %>% list(), xg_model = random_forward %>% list(), title = "random_forest" )


# write_rds(college_forests, 'data/processed/college_forests.rds')
models <- read_rds('data/processed/college_forests.rds')
models %>% 
  mutate(train = xgb_matrix(colleges_select %>%
                            select(- unitid, - instnm, - city, - latitude, - longitude, -stabbr),
                          "compl_rpy_5yr_rt") %>% list(),
         xg_test_mse = map2_dbl(xg_model, dat, xg_error, metric = "mse")
         
  )
