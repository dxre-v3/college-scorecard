# =================+ CLEANING THE DATA DICTIONARIES +==================== #

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)

# Get the Data ------------------------------------------------------------

# Save the file path
filename <- "data/unprocessed/CollegeScorecardDataDictionary.xlsx"
# Save the sheet names
sheets <- readxl::excel_sheets(filename)
# Save each sheet
x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
# Rename the sheets by there names (unnecesary tho)
names(x) <- sheets


# Clean and Save the Dictionaries -----------------------------------------



# ReadMe -----------------------------------------------------

readme <- read_xlsx('data/unprocessed/CollegeScorecardDataDictionary.xlsx',
          col_names = "Description") %>% 
  remove_empty() 

readme <- readme %>% 
  mutate(
    dictionary = map2_chr(Description, "“.*?”", str_extract),
    dictionary = map2_chr(dictionary, "”", str_remove),
    dictionary = map2_chr(dictionary, "“", str_remove),
    dictionary = ifelse(is.na(dictionary), "Change Log", dictionary),
    dictionary = ifelse(dictionary == "Download All Data", "Readme", dictionary)
  )

readme <- readme %>% 
  mutate(
        dictionary = map2_chr(dictionary, "_", replacement = " ", str_replace_all),
        dictionary = map2_chr(dictionary, "FieldOfStudy", 
                              replacement = "Field Of Study", str_replace),
        Dictionary = map_chr(dictionary, str_to_title),
        Location = row_number()
  ) %>% 
  select(Location, Dictionary, Description)

# ======== Only rerun if file needs to be re written ======= #
# write_rds(readme, 'data/processed/readme.rds')
# ========================================================== #

# Change Log -------------------------------------------------
changeLog <- x[[2]]

changeLog <- changeLog %>% 
  remove_empty()

# ======== Only rerun if file needs to be re written ======= #
# write_rds(changeLog, 'data/processed/changeLog.rds')
# ========================================================== #

# Glossary ---------------------------------------------------
glossary <- x[[3]]

# ======== Only rerun if file needs to be re written ======= #
# write_rds(glossary, 'data/processed/glossary.rds')
# ========================================================== #

# Institutions -----------------------------------------------
institutions <- x[[4]] %>% 
  clean_names() %>% 
  remove_empty()

institutions <- institutions %>% fill (
  name_of_data_element, dev_category, developer_friendly_name,
  api_data_type, variable_name, source
)

# ======== Only rerun if file needs to be re written ======= #
# write_rds(institutions, 'data/processed/institutions.rds')
# ========================================================== #

# Cohort Maps ------------------------------------------------
# I don't know what these are currently or why they are useful, so I will 
# instead save them one one set. 

cohort_map <- x[[5]]

recent_cohort_map <- x[[6]]

fos_cohort_map <- x[[8]]

cohort_maps <- tibble(
  institution_cm  = cohort_map %>% list(),
  recent_cm = recent_cohort_map %>% list(),
  study_cm = fos_cohort_map %>% list()
)

# ======== Only rerun if file needs to be re written ======= #
# write_rds(cohort_maps, 'data/processed/cohort_maps.rds')
# ========================================================== #


# Field of Study ---------------------------------------------


field_of_study <- x[[7]]%>% 
  clean_names()%>% 
  fill(name_of_data_element, dev_category,
       developer_friendly_name, api_data_type,
       source, variable_name)

# ======== Only rerun if file needs to be re written ========== #
# write_rds(field_of_study, 'data/processed/field_of_study.rds')
# ============================================================= #

