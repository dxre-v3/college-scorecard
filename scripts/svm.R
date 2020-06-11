source(file = "scripts/sample.R")
# -- Lasso - Model - Set -- #
colleges_train$compl_rpy_5yr_rt %>% skimr::skim()

college_vfold <- colleges_train %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))

college_class <- colleges_train %>% 
  mutate(
    compl_rpy_5yr_rt = case_when(compl_rpy_5yr_rt < .644 ~ "q1",
                                 between(compl_rpy_5yr_rt, .644, .771) ~ "q2",
                                 between(compl_rpy_5yr_rt, .771, .898) ~ "q3",
                                 compl_rpy_5yr_rt > .898 ~ "q4",
                                 T ~ "your areas are off"),
    compl_rpy_5yr_rt = compl_rpy_5yr_rt %>% as.factor()
    
  ) %>% 
    select(- unitid, - instnm, - city, - latitude, - longitude, -stabbr) %>% 
  rsample::vfold_cv(5) %>% 
  mutate(train = map(splits, training),
         test = map(splits, testing))

library(e1071)
library(pROC)

college_class %>% 
  crossing(tibble(cost = list(0.1, 1, 10))) %>%
    mutate(svm_linear = map2(.x = train, .y = cost,
                           .f = function(x, y) svm(compl_rpy_5yr_rt ~ ., x, kernel = "linear", cost = y)), # fit svm model
         svm_linear_summary = map(svm_linear, summary)
    )


