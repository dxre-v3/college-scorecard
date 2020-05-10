# =================+     EXPLORATION FOR DATA MEMO    +==================== #


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(skimr)


# Read data  --------------------------------------------------------------

colleges <- read_csv('data/unprocessed/Most-Recent-Cohorts-All-Data-Elements.csv')  %>% clean_names()

classes <- read_csv('data/unprocessed/Most-Recent-Field-Data-Elements.csv')
classes %>% 
  clean_names() %>% 
  filter(is.na())

colleges %>% skim()
colleges %>% names


# NA Rate -----------------------------------------------------------------

colleges %>%
  map_df(is.na) %>% 
  map_df(mean) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "NA_rate") %>%  
  filter(NA_rate != 0)


# Type of variable --------------------------------------------------------

colleges %>%
  map_df(n_distinct) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "num_outcomes") %>%  
  filter(num_outcomes == 1)



institutions %>% 
  select(variable_name, name_of_data_element) %>% 
  remove_empty() %>% view()


# Explorations ------------------------------------------------------------

colleges %>% 
  select(instnm, stabbr, adm_rate)

colleges %>% 
  filter(adm_rate != "NULL") %>% 
  mutate(no_engineering = pcip14 == "0") %>% 
  filter(no_engineering == T) %>% 
  ggplot(aes(as.numeric(adm_rate))) +
  geom_density() + 
  labs(
    x = "Admission Rate",
    y = "Density",
    caption = "Density of Institutions without engineering graduates",
    title = 'Lack of Engineering'
  ) +
  theme_minimal()

colleges %>% 
  filter(adm_rate != "NULL") %>% 
  mutate(no_engineering = pcip14 == "0") %>% 
  filter(no_engineering == F) %>% 
  ggplot(aes(as.numeric(adm_rate), as.numeric(pcip14))) +
  geom_point(alpha = 0.5) + 
  labs(
    x = "Admission Rate",
    y = "Percentage of Engineering Degrees",
    caption = "Percentage of Engineering Degrees by Admission Rate",
    title = 'Engineering by Admission Rate'
  ) +
  theme_minimal()


colleges %>% 
  filter(adm_rate != "NULL") %>% 
  mutate(med_mean_diff = as.numeric(mn_earn_wne_p10) -  as.numeric(md_earn_wne_p10)) %>% 
  ggplot(aes(as.numeric(adm_rate), med_mean_diff)) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Admission Rate",
    y = "Dollar Amount",
    title = "Mean and Median Earnings by Admission Rate",
    caption = "Difference in the average mean and median earnings of student after 10 years"
  ) + 
  theme_minimal()
  

MN_EARN_WNE_P10 
MD_EARN_WNE_P10 
