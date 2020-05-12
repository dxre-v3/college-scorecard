# EXPLORATORY DATA ANALYSIS -----------------------------------------------
# Load in Data and Libraries

library(tidyverse)

colleges <- read_rds('data/processed/colleges.rds')

terms <- read_rds('data/processed/terms.rds')


# Q1: Should I use my response variable? ----------------------------------

colleges$compl_rpy_5yr_rt

colleges$compl_rpy_5yr_rt %>% skimr::skim()
# Ok so we don't have a full dataset, there are a bunch 
# of missing values. These could either be privacy supressed 
# or NULL. 

colleges %>% 
  filter(control == 1) %>% 
  select(compl_rpy_5yr_rt) %>% 
  skimr::skim()
# Private colleges have a lower rate of missingness

colleges %>% 
  filter(control == 2) %>% 
  select(compl_rpy_5yr_rt) %>% 
  skimr::skim()
# Public Univerisities have a missing rate of about 21%. That's a lot.
# Let's see if the info is you know missing for anything else.

colleges %>% 
  filter(is.na(compl_rpy_5yr_rt)) %>% 
  skimr::skim() 
# It's missing for a lot of things. Ok. Lets see if there's a reason;
# are all these colleges in the same region? 

colleges %>% 
  filter(is.na(compl_rpy_5yr_rt)) %>% 
  count(stabbr) %>%
  filter(n>5) %>% 
  ggplot(aes(stabbr, n)) + 
  geom_col()+
  coord_flip() +
  theme_minimal()+
  labs(x = "State",
       y = "Number of institutions that did not report the reponse variable")

colleges %>%
  mutate(is_missing = is.na(compl_rpy_5yr_rt)) %>% 
  group_by(stabbr) %>%
  summarise(
    n = mean(is_missing)
    ) %>% 
  filter(between(n,.10,.25) )%>% 
  ggplot(aes(stabbr, n)) + 
  geom_col()+
  coord_flip() +
  theme_minimal()+
  labs(x = "State",
       y = "% of institutions that did not report the reponse variable")

colleges %>%
  mutate(is_missing = is.na(compl_rpy_5yr_rt)) %>% 
  group_by(stabbr) %>%
  summarise(
    n = mean(is_missing)
    ) %>% 
  filter((n >.25) )%>% 
  ggplot(aes(stabbr, n)) + 
  geom_col()+
  coord_flip() +
  theme_minimal()+
  labs(x = "State",
       y = "% of institutions that did not report the reponse variable")
  
# Given all these graphs, we will seperate out the data into two categories, 
# those colleges that reported the 5 year repayment rate and those that didn't.

small_colleges <- colleges %>% filter(!is.na(compl_rpy_5yr_rt))

# Q2: Should I Keep Private Institutions in? ------------------------------

priv_means <- small_colleges %>% 
  filter(control == 1) %>% 
  map_df( mean) %>%
  pivot_longer(adm_rate:g12mn, names_to = "cols", values_to = "priv") %>% 
  select(cols, priv)

pub_means <- small_colleges %>% 
  filter(control == 2) %>% 
  map_df( mean) %>%
  pivot_longer(adm_rate:g12mn, names_to = "cols", values_to = "pub") %>% 
  select(cols, pub)

priv_means %>% left_join(pub_means) %>% 
  pivot_longer(c(pub, priv), names_to = "type", values_to = "mean") %>% 
  filter(!is.na(mean)) %>% 
  ggplot(aes(mean, cols)) +
  geom_point(aes(color = type))


small_colleges %>% 
  ggplot(aes(compl_rpy_5yr_rt)) +
  geom_density() +
  theme_minimal()+
  facet_wrap(~control)

# How much data do I have if I remove private colleges?

small_colleges %>% 
  filter(control == 1) %>% 
  dim()

# I think the data is similar enough that I can keep both in. 



# sampling numbers ---------------------------------------------------

# Run ctl-shft-F10 to restart R
source("scripts/sample.R")

# Due to the number of the variables I have, I want to have as much data as possible
# final model selection and performance so we take a 70, 15, 15 split across the 
# data. Instead of exploring a seperate set of data then, we will explore the '
# training set. 



# EXPLORATORY ANALYSIS ----------------------------------------------------

colleges_train %>% 
  skimr::skim()

# Response Variable

colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt)) +
  geom_density() +
  ggthemes::theme_clean()

colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt)) +
  geom_histogram(binwidth = 0.02) +
  ggthemes::theme_clean()

colleges_train %>% 
  skimr::skim(compl_rpy_5yr_rt)

# ============ ugds explororation

ugds <- str_match(colleges_train %>% names, "^ugds.*")
ugds <- ugds[!is.na(ugds)]
ugds_eda <- colleges_train %>% 
  select(c(ugds,compl_rpy_5yr_rt) )

# By Ethnicity
ugds_eda %>% 
  pivot_longer(c(-compl_rpy_5yr_rt, -ugds_men, -ugds_women), values_to = "percent_eth", names_to = "ethnicity") %>% 
  ggplot(aes(compl_rpy_5yr_rt, percent_eth)) +
  geom_point(aes(color = ethnicity))+
  facet_wrap(~ethnicity) +
  theme_classic()

# By male and female
ugds_eda %>% 
  pivot_longer(c(ugds_men, ugds_women), values_to = "percent_sex", names_to = "gender") %>% 
  ggplot(aes(compl_rpy_5yr_rt, percent_sex)) +
  geom_point(aes(color = gender))+
  facet_wrap(~gender)+
  theme_classic()


eth_labels <-terms %>% 
  select(name = name_of_data_element, var = variable_name ) %>% 
  mutate(
    name = str_remove(name, "(^Total.*who are)|(^Total.*whose race is)"),
    var = str_to_lower(var),
    name = str_to_title(name),
    name = str_trim(name)
  ) %>% 
  filter(str_detect(var, "ugds"))

# ========== Number of branches
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, numbranch)) +
  geom_jitter()+
  theme_classic()

colleges_train %>% 
  filter(numbranch < 5) %>% 
  ggplot(aes( as.factor(numbranch), compl_rpy_5yr_rt)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.4, color = "orange") +
  theme_classic() 


# ========== Locale
colleges_train %>% 
  ggplot(aes(as.factor(locale), compl_rpy_5yr_rt) )+
  geom_boxplot() +
  theme_classic()

# ========== flags
flag_type <- function(col){
  n_distinct(col) == 2
}

flags <-  c(colleges_train %>% 
  select_if(flag_type) %>% names)

flags

flag_names <- terms %>% 
  select(name = name_of_data_element, var = variable_name ) %>% 
  mutate(
    name = str_remove(name, "(^Flag for)"),
    name = str_replace(name, "(^Control)", "Type"),
    var = str_to_lower(var),
    name = str_to_title(name),
    name = str_trim(name)
  ) %>% 
  filter(var %in% flags) %>% 
  distinct(name, .keep_all = TRUE)

colleges_train %>% 
  select(flags, compl_rpy_5yr_rt) %>% 
  pivot_longer(-compl_rpy_5yr_rt, values_to = "class", names_to = "flags") %>% 
  ggplot(aes(as.factor(class), compl_rpy_5yr_rt)) +
  geom_boxplot(aes(color = flags))+
  facet_wrap(~flags) +
  theme_classic()


# ========== Religion
colleges_train %>% 
  mutate(rel_flag = if_else(relaffil < 0, "None", "Affil" )) %>% 
  ggplot(aes(rel_flag, compl_rpy_5yr_rt)) +
  geom_boxplot() +
  geom_jitter(color = "pink", alpha = 0.4) +
  theme_classic()

colleges_train %>% 
  mutate(rel_flag = if_else(relaffil < 0, "None", "Affil" )) %>% 
  count(rel_flag)

# ========== sat avg
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, sat_avg))+
  geom_point() +
  theme_classic()

# ========== Percent of Degrees

degree <- str_match(colleges_train %>% names, "^pcip.*")
degree <- degree[!is.na(degree)]

deg_names <- terms %>%
  select(name = name_of_data_element, var = variable_name ) %>% 
  mutate(
    name = str_remove(name, "^Percentage.*in "),
    name = str_extract(name, "^.*?[,.]"),
    name = str_remove(name, "[,.]"),
    name = str_remove(name, "(And Literature/Letters)|(And Related Programs)"),
    var = str_to_lower(var),
    name = str_to_title(name),
    name = str_trim(name)
  ) %>% 
  filter(str_detect(var, "^pcip.*")) 

colleges_train %>% 
  select(c(degree, compl_rpy_5yr_rt)) %>% 
  pivot_longer(c(-compl_rpy_5yr_rt), values_to = "percent", names_to = "degree") %>% 
  ggplot(aes(compl_rpy_5yr_rt, percent)) +
  geom_point(aes(color = degree))+
  facet_wrap(~degree) +
  theme_classic()


eth_labels <-terms %>% 
  select(name = name_of_data_element, var = variable_name ) %>% 
  mutate(
    name = str_remove(name, "(^Total.*who are)|(^Total.*whose race is)"),
    var = str_to_lower(var),
    name = str_to_title(name),
    name = str_trim(name)
  ) %>% 
  filter(str_detect(var, "ugds"))

# ========== price
colleges_train %>%  
  ggplot(aes( compl_rpy_5yr_rt, npt4_priv) )+
  geom_point() +
  theme_classic()

colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, npt4_pub) )+
  geom_point() +
  theme_classic()

# === Revenue
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, tuitfte) )+
  geom_point() +
  theme_classic()


# === proffs
# Salary
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, avgfacsal) )+
  geom_point() +
  theme_classic()
# Full Time
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, pftfac) )+
  geom_point() +
  theme_classic()

# =============== AID 
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, pctpell) )+
  geom_point() +
  theme_classic()

# Retention
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, ret_ft4) )+
  geom_point() +
  theme_classic()

# Loan rate
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, pctfloan) )+
  geom_point(aes(color = as.factor(control)), alpha = 0.5) +
  theme_classic()

# age
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, ug25abv) )+
  geom_point() +
  theme_classic()

colleges_train %>% count(iclevel)

# Enrollement 
colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, ug12mn) )+
  geom_point() +
  theme_classic()

colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, g12mn) )+
  geom_point() +
  theme_classic()


colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, adm_rate) )+
  geom_point() +
  theme_classic()


# Money 

colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, gt_28k_p6) )+
  geom_point() +
  theme_classic()


colleges_train %>% 
  ggplot(aes(compl_rpy_5yr_rt, fsend_count) )+
  geom_point() +
  theme_classic()

# correlation is not particularly useful here :( 

# not_flag <- function(col){
#   n_distinct(col) > 3
# }
# 
# colleges_train %>% 
#   select_if(is.numeric) %>% 
#   select_if(not_flag) %>% 
#   select(-unitid, -sch_deg, -numbranch, -locale,
#          -latitude, -longitude, -relaffil) %>% 
#   cor() %>% 
#   View()
# 
# 
# 
#   corrplot::corrplot()

 colleges_train %>% select(compl_rpy_5yr_rt, sat_avg) %>% cor(use="complete.obs")
 colleges_train %>% select(compl_rpy_5yr_rt, npt4_pub)%>% cor(use="complete.obs")
 colleges_train %>% select(compl_rpy_5yr_rt, npt4_priv) %>% cor(use="complete.obs")
  colleges_train %>% select(compl_rpy_5yr_rt, pctpell) %>% cor(use="complete.obs")
