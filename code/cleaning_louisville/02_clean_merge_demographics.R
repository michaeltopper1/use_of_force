## Purpose of script: merges the officer data and officer demographics
##
## Author: Michael Topper
##
## Date Last Edited: 2022-08-18
##

library(tidyverse)
library(lubridate)


# merging the two officer data sets for more covariates -------------------
## this is the old officer information. however, it contains useful information such as the first/last name and education
## this information should be current through 2021
officers_old <- readxl::read_excel("raw_data/louisville/police_demographics/demographics_louisville.xlsx")

officers <- read_csv("raw_data/louisville/police_demographics/demographics_aug2022.csv") %>% 
  janitor::clean_names() %>% 
  rename(badge = aoc_code) %>% 
  mutate(badge = as.character(badge))




# cleaning officers_old ---------------------------------------------------

officers_old <- officers_old %>% 
  mutate(across(c(appointdate, dateoftermination), ~lubridate::ymd(.))) %>% 
  rename("appoint_date" = appointdate, "termination_date" = dateoftermination,
         "first_name" = fname, "last_name" = lname, "education_level" = educationlevel) %>% 
  extract(education_level, "education_level_letter", "(^[A-Z])", remove = F) %>% 
  mutate(college = ifelse(education_level_letter == "G", 1, 0),
         some_college = ifelse(education_level_letter == "F" | education_level_letter == "D" | education_level_letter == "H", 1,0),
         graduate_degree = ifelse(education_level_letter == "I" | education_level_letter == "K", 1, 0),
         high_school_grad = ifelse(education_level_letter == "C" | education_level_letter == "E",1 ,0), 
         unknown_schooling = ifelse(education_level_letter == "A" | is.na(education_level_letter),1, 0)) %>% 
  mutate(across(where(is.character), ~str_to_lower(.) %>% str_trim())) 



# cleaning new officers information ---------------------------------------

officers <- officers %>% 
  mutate(officer_female = ifelse(officer_sex == "F", 1, 0)) %>% 
  mutate(officer_black = ifelse(officer_race == "B", 1, 0),
         officer_white = ifelse(officer_race == "W", 1, 0),
         officer_asian = ifelse(officer_race == "A", 1, 0),
         officer_hispanic = ifelse(officer_race == "H", 1, 0))


# merging the information -------------------------------------------------

all_officers <- officers %>% 
  left_join(officers_old) %>% 
  relocate(first_name, last_name, badge) 


 

# getting rid of unnecessary information ----------------------------------
## first, getting rid of officers with age == 0. These entries have no useful info in them.

all_officers <- all_officers %>% 
  filter(officer_age != 0) %>% 
  mutate(across(where(is.character), ~str_to_lower(.) %>% str_trim()))

all_officers %>% 
  write_csv("created_data/louisville/officer_demographics_cleaned.csv")
