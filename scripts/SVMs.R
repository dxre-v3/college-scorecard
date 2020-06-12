# SUPPORT VECTOR MACHINE - - - Correlated Data ----------------------------
# Explanation:

# Although this is more or less a regression problem, I was curious to 
# know if a classification model would do better. Below I used the
# correlated variables to and ran a model with the form response by 
# remaining variables

# Includes linear, polynomial and radial kernels 

# Data Set Up -------------------------------------------------------------

# Load the data, library and seed
source(file = "scripts/sample.R")
# -- Specific - Libraries - Set -- #
library(e1071)

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
#     (as a factor)
# -------------------------------------------------------------------------

college_vfold <- colleges_train %>% 
  mutate(
    # create a factor (svm does not run on non-factors)
    compl_rpy_5yr_rt = case_when(compl_rpy_5yr_rt < .644 ~ "q1",
                                 between(compl_rpy_5yr_rt, .644, .771) ~ "q2",
                                 between(compl_rpy_5yr_rt, .771, .898) ~ "q3",
                                 compl_rpy_5yr_rt > .898 ~ "q4",
                                 T ~ "your areas are off"),
    compl_rpy_5yr_rt = compl_rpy_5yr_rt %>% as.factor()
  )%>%
  # select the selected vars
  select(all_of(selection)) %>%
  # Fold
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))

# -------------------------------------------------------------------------
# ERROR RATE FUNCTION
# -------------------------------------------------------------------------

error_rate <- function(data, model){
  data %>% 
    # make predictions then get how close the data is to the other 
    mutate(pred_prob = predict(model, newdata = data, type = "response"),
           error = pred_prob != compl_rpy_5yr_rt) %>% 
    pull(error) %>% 
    mean()
}

# -------------------------------------------------------------------------
# LINEAR KERNEL
# -------------------------------------------------------------------------

# svm train with different cost 
linear_training <- college_vfold %>% 
  crossing(tibble(cost = c(0.1, 1, 10))) %>%
    mutate(svm_linear = map2(.x = train, .y = cost,
                           .f = function(x, y) svm(compl_rpy_5yr_rt ~ ., x, 
                                                   kernel = "linear", cost = y)), # fit svm model
         summary = map(svm_linear, summary)
    )

# get the error rate across costs
linear_training %>% 
  mutate(
    error = map2_dbl(test,svm_linear, error_rate)
  ) %>% 
  group_by(cost) %>% 
  summarise(error_rate = mean(error))

# Linear Results: 3 x 2
#    cost error_rate
#   <dbl>      <dbl>
# 1   0.1      0.355
# 2   1        0.345
# 3  10        0.350

# -------------------------------------------------------------------------
# POLYNOMIAL KERNEL
# -------------------------------------------------------------------------

# svm train with different cost 
poly_training <- college_vfold %>% 
  crossing(tibble(cost = c(0.1, 1, 10, 50))) %>%
    mutate(svm_poly = map2(.x = train, .y = cost,
                           .f = function(x, y) svm(compl_rpy_5yr_rt ~ ., x, 
                                                   kernel = "polynomial", cost = y)), # fit svm model
         summary = map(svm_poly, summary)
    )

# get the error rate across costs
poly_training %>% 
  mutate(
    error = map2_dbl(test, svm_poly, error_rate)
  ) %>% 
  group_by(cost) %>% 
  summarise(error_rate = mean(error))

# # Polynomial Results: 4 x 2
#    cost error_rate
#   <dbl>      <dbl>
# 1   0.1      0.436
# 2   1        0.397
# 3  10        0.384
# 4  50        0.414


# -------------------------------------------------------------------------
# RADIAL KERNEL
# -------------------------------------------------------------------------

# svm train with different cost 
radial_training <- college_vfold %>% 
  crossing(tibble(cost = c(0.1, 1, 10, 50))) %>%
    mutate(svm_rad = map2(.x = train, .y = cost,
                           .f = function(x, y) svm(compl_rpy_5yr_rt ~ ., x, 
                                                   kernel = "radial", cost = y)), # fit svm model
         summary = map(svm_rad, summary)
    )

# get the error rate across costs
radial_training %>%
  mutate(
    error = map2_dbl(test,svm_rad, error_rate)
  ) %>% 
  group_by(cost) %>% 
  summarise(error_rate = mean(error))

# Radial Results: 4 x 2
#    cost error_rate
#   <dbl>      <dbl>
# 1   0.1      0.495
# 2   1        0.356
# 3  10        0.375
# 4  50        0.404