# Data Sample -------------------------------------------------------------
library(tidyverse) 
library(rsample)
set.seed(36802911)
colleges <- read_rds('data/processed/colleges.rds')

colleges <- colleges %>% filter(!is.na(compl_rpy_5yr_rt))



# create a function that take a data column with NAs
# and spits out a column where the nas have been filled 

noNA = function(array){
  # check the type of array
  type <- typeof(array)
  n <- array %>% n_distinct()
  if(type == "double" & n < 6){
    naless = array %>% replace_na(-1)
  }else{
    naless = array
  }
  return(naless)
}

# check what the function is doing 
# save data 
colleges <- colleges %>% 
  map_df(noNA)

# looking at the data
# colleges %>% 
#   map_df(n_distinct) %>% 
#   pivot_longer( -unitid, names_to = "var", values_to = "n") %>% 
#   arrange(n) %>% view()



terms <- read_rds('data/processed/terms.rds')


# Splitting all the the data ----------------------------------------------------------
# ---------- Created modeling data set: 70%
colleges_split_initial <- colleges %>% initial_split(7/10)
# Training dataset for performance
colleges_train <-colleges_split_initial %>% training()

# ----------- Create (model selection) holdout dataset: 15% 
colleges_split_holdouts <- colleges_split_initial %>% testing() %>% initial_split(1/2)
# Validataion dataset for selection
colleges_select <- colleges_split_holdouts %>% training()
# Validataion dataset for selection
colleges_perform <- colleges_split_holdouts %>% testing()

