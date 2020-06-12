# RANDOM FORESTS/BOOSTED DT ---------------------------------------
# Explanation:

# In this file I run boosted and random forests against the data. 


# Data Set Up -------------------------------------------------------------

# Load the data, library and seed
source(file = "scripts/sample.R")

# -- Specific - Libraries - Set -- #
library(xgboost)

# -------------------------------------------------------------------------
# XGBOOST FUNCTIONS
#         (mostly copied off of the DSM)
# -------------------------------------------------------------------------

# create d.matrix (matrix necessary for forests)
xgb_matrix <- function(dat, outcome){
    pred <- dat[[outcome]]
    mat <- dat %>% dplyr::select(-outcome) %>% # encode on full boston df
      onehot::onehot() %>% # use onehot to encode variables
      predict(dat) # get OHE matrix
    return(xgb.DMatrix(data = mat, 
                       label = pred))
}

# -------------------------------------------------------------------------

# get error for xgboost forests
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


# -------------------------------------------------------------------------
# TRAINING DATA SET
# -------------------------------------------------------------------------

numeric_only <- colleges_train %>% 
  select(- unitid, - instnm, - city, - latitude, - longitude, -stabbr)

# exttra<- xgb_matrix(no_char_coll, "compl_rpy_5yr_rt" )

# -------------------------------------------------------------------------

xg_college <- numeric_only %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing)) %>% 
  mutate(train_mat = map(train, xgb_matrix, outcome = "compl_rpy_5yr_rt"),
         test_mat = map(test, xgb_matrix, outcome = "compl_rpy_5yr_rt"))


# -------------------------------------------------------------------------
# RANDOM FORESTS 
# -------------------------------------------------------------------------

# Random Forests, untrained with 200 nrounds
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

# Get error rate

(xg_random_forest <- xg_random_forest %>% 
  mutate(
    xg_train_mse = map2_dbl(xg_model, train_mat, xg_error, metric = "mse"),
    xg_test_mse = map2_dbl(xg_model, test_mat, xg_error, metric = "mse"),
    var = map_dbl(test, ~var(.x$compl_rpy_5yr_rt)),
    percent = xg_test_mse/var
    )) 

# A tibble: 5 x 4
#   xg_train_mse xg_test_mse    var percent
#          <dbl>       <dbl>  <dbl>   <dbl>
# 1  0.000000183     0.00333 0.0156   0.214
# 2  0.000000180     0.00338 0.0150   0.225
# 3  0.000000180     0.00253 0.0141   0.180
# 4  0.000000181     0.00361 0.0210   0.172
# 5  0.000000181     0.00264 0.0143   0.185

xg_random_forest %>% 
  summarise(
    test_mse = mean(xg_train_mse),
    error_rate = mean(percent)
  )

#      test_mse error_rate
#         <dbl>      <dbl>
# 1 0.000000181      0.195

# So using a form of random forests via xgboost on the data produced pretty good results 
# using the 110 varaiables. lets see if we can get the importance on top of that. 


# -------------------------------------------------------------------------
# TOP VARIABLES 
# -------------------------------------------------------------------------

rf_names <- xg_random_forest %>% pluck("train", 1) %>% colnames() %>% as.vector()
rf_m1 <- xg_random_forest %>% pluck("xg_model", 1)

xgb.importance(rf_names, model = rf_m1)%>% as.tibble() %>% 
  arrange(desc(Gain)) %>% 
  head(10) %>% 
  pluck(1)

xg_random_forest %>% pluck("xg_model", 1)

# Well. So i got importance but I dont understand it at all.... ok so i kinda understand it
# We're only going to use gain here. 


# FUNCTION to get the top variables ---------------------------------------

xg_topn = function(df, index, length = 5){
  names <- df %>% pluck("train", index) %>% colnames() %>% as.vector()
  model <- models %>% pluck("xg_model", index)
  
  xgb.importance(names, model = model)%>% as_tibble() %>% 
  arrange(desc(Gain))%>% 
  head(length) %>% 
  pluck(1)
}

# -------------------------------------------------------------------------

# GET top 5 categories for each forest
xg_random_forest <- xg_random_forest %>% 
  mutate(num = dplyr::row_number(),
    top5_vars = map(.x = num, ~ xg_topn(xg_random_forest, index = .x, 10)))

# Compare the forests

xg_random_forest %>% unnest(top5_vars) %>% 
  count(top5_vars)

# They're all the same so...  

# -------------------------------------------------------------------------
# BOOSTED MODELS
# -------------------------------------------------------------------------

# create the training data set (crossed with learning rates)
xgb_college <- numeric_only %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing)) %>% 
   crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>%
  mutate(train_mat = map(train, xgb_matrix, outcome = "compl_rpy_5yr_rt"),
         test_mat = map(test, xgb_matrix, outcome = "compl_rpy_5yr_rt"))

# save boosted model 
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
  
#  GET the MSE (there are lot so I didn't save the results in the file)
(xg_boost <- xg_boost %>% 
  mutate(
    xg_train_mse = map2_dbl(xg_model, train_mat, xg_error, metric = "mse"),
    xg_test_mse = map2_dbl(xg_model, test_mat, xg_error, metric = "mse")
    ))

# View the average results
xg_boost %>% 
  group_by(learn_rate) %>% 
  summarise(train_mse = mean(xg_train_mse),
            test_mse = mean(xg_test_mse)) %>% view


# It looks like the best learning rate may be the 18th one. 
# However i don't want to over fit so I will also keep 15 and 16
# 1.971228e-03 <- 0.0434
# 6.543189e-03 <- .0104
# 7.209327e-02 <best with 0.00332

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# SAVE FORESTS FOR SELECTION
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Create the matrix for the random forests
forest_matrix <- xgb_matrix(numeric_only, "compl_rpy_5yr_rt")

# save the model
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

# Save the three best learn rates along with the training data
 college_forests <- tibble(
    nums = c(1.971228e-03, 6.543189e-03, 7.209327e-02),
    dat = c(forest_matrix)
  ) %>% 
   # fit the boosted models to the data 
    mutate(xg_model = map2(.x = dat, .y = nums, 
                          .f = function(x, y) xgb.train(params = list(eta = y,
                                                                      depth = 10,
                                                                      objective = "reg:squarederror"),
                                                     data = x,
                                                     nrounds = 200
                                                     )
                        )
  )

# Add the random forests 
 
college_forests <- college_forests %>%
  mutate(title = str_c("boosted_", row_number())) %>% 
  add_row(nums = NA, dat = forest_matrix %>% list(), xg_model = random_forward %>% list(), title = "random_forest" )


# -- save data so you don't have to save every time ---------------------------
# write_rds(college_forests, 'data/processed/college_forests.rds')
