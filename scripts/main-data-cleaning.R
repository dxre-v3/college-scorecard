library(tidyverse)
library(janitor)

# Read in main data set

initial_read <- read_csv('data/unprocessed/Most-Recent-Cohorts-All-Data-Elements.csv',
                         na = c("", "NA", "NULL"))

privsupress <- problems(initial_read)
terms <- read_rds('data/processed/institutions.rds')

privsupress <- privsupress %>% 
  left_join(terms, by = c("col" = "variable_name")) %>% 
    mutate(recorded = "NA") %>% 
  select(name_of_data_element, variable_name = col, api_data_type, recorded, actual)

# Keeping track of how things are saved
# ============== Only re run if necessary ======================
# write_rds(privsupress, 'data/processed/privacy_supressed.rds')
#================================================================


# Try with several custom NA values ---------------------------------------

second_read <- read_csv('data/unprocessed/Most-Recent-Cohorts-All-Data-Elements.csv',
                         na = c("", "NA", "NULL", "PrivacySuppressed"))

# New Problems
problems(second_read) %>% 
  left_join(terms, by = c("col" = "variable_name")) %>% 
  select(name_of_data_element, variable_name = col, api_data_type, expected, actual) %>% 
  view()


# Change guess max size ---------------------------------------------------

colleges <- read_csv('data/unprocessed/Most-Recent-Cohorts-All-Data-Elements.csv',
                         na = c("", "NA", "NULL", "PrivacySuppressed"),
                        guess_max = 6000)


# Our data is perfectly fine now, but we need to get the categories we actual want to work with
# To do that we are actually going to spend a bit of time with our terms dataset


# Variable choices --------------------------------------------------------
terms %>% count(dev_category)

# Why do we have a bunch of NA values?
terms %>% 
  filter(is.na(dev_category)) %>% 
  view

# It looks like they are the levels of certain variables. I will fill them later
# if need be. For now, they are fine. 


# Root variables ----------------------------------------------------------

terms %>% 
  filter(dev_category == "root") %>% 
  view

# Useful = UNITID, 	LATITUDE, LONGITUDE 


# Repayment ---------------------------------------------------------------

terms %>% 
  filter(dev_category == "repayment") %>% 
  view

term_small <- terms %>% 
  select(name_of_data_element, dev_category, variable_name, label)

# consider all the variables to select the most interesting ones -----------------
term_small %>% filter(dev_category == "academics") %>%   view
term_small %>% filter(dev_category == "admissions") %>%   view
term_small %>% filter(dev_category == "aid") %>%   view
term_small %>% filter(dev_category == "completion") %>%   view
term_small %>% filter(dev_category == "cost") %>%   view
term_small %>% filter(dev_category == "earnings") %>%   view
term_small %>% filter(dev_category == "repayment") %>%   view
term_small %>% filter(dev_category == "root") %>%   view
term_small %>% filter(dev_category == "school") %>%   view
term_small %>% filter(dev_category == "student") %>%   view

# create sets of strings to save the interesting columns 
academics <- str_c("PCIP", c(13, 14, 23, 24, 26:28, 40, 42, 45, 50:54))
select_1 <- c("ADM_RATE", "SAT_AVG", "PCTPELL", "PCTFLOAN", "GRAD_DEBT_MDN",
              "C150_L4", "NPT4_PUB", "NPT4_PRIV", "COMPL_RPY_5YR_RT", "UNITID", 
              "LONGITUDE", "LATITUDE", "MD_EARN_WNE_P6", "MN_EARN_WNE_P6",
              "COUNT_NE_P6", "COUNT_WNE_P6") 
earnings <- str_match(term_small$variable_name, ".*_P6")
earnings <- earnings[!is.na(earnings)]
school <- c("INSTNM", "CITY", "STABBR", "SCH_DEG", "NUMBRANCH", 
            "PREDDEG", "CONTROL", "LOCALE", "RELAFFIL", "AVGFACSAL",
            "PFTFAC", "ICLEVEL", "TUITFTE", "WOMENONLY", "MENONLY", "PBI",
            "HBCU", "ANNHI", "TRIBAL", "AANAPII", "NANTI")

# Student has a lot of matches so we will create it's own data set to filter from
student_set <- term_small %>% filter(dev_category == "student")
# 
# Save the regex as a string and then filter for the names 
match_vars <- "(^PAR_ED.*)|(^UGDS_.*)|(^APPL_.*)|(^DEP_INC.*)|(^FSEND.*)"
par <- str_match(student_set$variable_name, match_vars)
par <- par[!is.na(par)]
# add those names to the other chosen varaibles and save
student <- c("RET_FT4", "UG25ABV", "AGE_ENTRY", "FEMALE", 
             "VETERAN", "MD_FAMINC", "FAMINC", "UG12MN", "G12MN", par)


# save the terms 
selected_vars <- terms %>% 
  filter(variable_name %in% c(academics, select_1, earnings, school, student))

# ++++++++ Saving the short version of the data ++++++++++++
# write_rds(selected_vars, "data/processed/terms.rds")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Get the selected variable names
variable_filter <- selected_vars$variable_name

# Filter on those names 
small_colleges <- colleges %>% 
  select(variable_filter)

# Take a quick looks at the data 
small_colleges %>% skimr::skim()

# Remove the empty columns 
small_colleges <- small_colleges %>% 
  janitor::remove_empty(which = "cols") %>% 
  janitor::clean_names()

small_colleges <- small_colleges %>% 
  filter(preddeg == 3 & control %in% c(1, 2))

# -------------------------------------------------------------------------
# ========================== SAVE CLEANED DATA ============================
# -------------------------------------------------------------------------
# write_rds(small_colleges, "data/processed/colleges.rds")
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



# terms %>% 
#   filter(label == "Private nonprofit")
