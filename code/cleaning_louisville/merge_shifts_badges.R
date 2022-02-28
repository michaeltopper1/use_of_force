## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-22
##

library(tidyverse)
library(lubridate)


# cleaning shifts data ----------------------------------------------------

sheets <- readxl::excel_sheets("raw_data/louisville/shifts_worked/work_schedules_louisville.xlsx")

shifts <- map_df(sheets, ~readxl::read_excel("raw_data/louisville/shifts_worked/work_schedules_louisville.xlsx", sheet = .) %>% 
      janitor::clean_names() %>% mutate(badge = as.character(badge)))


shifts <- shifts %>% 
  mutate(start_date = floor_date(startdate, unit = "hours"),
         end_date = floor_date(enddate, unit = "hours"),
         shift_length = seconds(end_date - start_date)) %>% 
  mutate(shift_length = seconds_to_period(shift_length), .before = 1) %>% 
  mutate(shift_hours = hour(shift_length), .before =1 ) %>% 
  mutate(shift_year = lubridate::year(start_date),
         shift_month = lubridate::month(start_date),
         shift_day = lubridate::day(start_date)) 

officer_badges <- shifts %>% 
  distinct(badge) %>% pull()


# cleaning officer demographics -------------------------------------------

officers <- readxl::read_excel("raw_data/louisville/police_demographics/demographics_louisville.xlsx")


officers <- officers %>% 
  mutate(across(c(appointdate, dateoftermination), ~lubridate::ymd(.))) %>% 
  rename("appoint_date" = appointdate, "termination_date" = dateoftermination,
         "first_name" = fname, "last_name" = lname, "education_level" = educationlevel)

## shows that there are 3 duplicates: 1 for one guy who retired and came back, another 2 for duplicates. I collapse the 2 duplicates and distinct the 1st guy
officers <- officers %>% 
  distinct(badge, first_name, last_name, rank, education_level, appoint_date) 


# merging with shifts data ------------------------------------------------
## apears there are an additional 11 rows that are added where there are multiple matches..negligible in my opinion - investigate
shifts <- shifts %>% 
  distinct() %>% ## removes duplicate shifts
  left_join(officers, by = c("badge"))


# creating additional necessary columns -----------------------------------
## experience will be age at jan 1, 2020
## education will be the highest level of educational attainment split by college, some college, high school, graduate, n
shifts <- shifts %>% 
  mutate(start_hour = hour(startdate), end_hour = hour(enddate)) %>% 
  extract(education_level, "education_level_letter", "(^[A-Z])", remove = F) %>% 
  mutate(college = ifelse(education_level_letter == "G", 1, 0),
         some_college = ifelse(education_level_letter == "F" | education_level_letter == "D" | education_level_letter == "H", 1,0),
         graduate_degree = ifelse(education_level_letter == "I" | education_level_letter == "K", 1, 0),
         high_school_grad = ifelse(education_level_letter == "C" | education_level_letter == "E",1 ,0), 
         unknown_schooling = ifelse(education_level_letter == "A" | is.na(education_level_letter),1, 0)) %>% 
  mutate(officer_tenure = as_date("2020-01-01") - appoint_date) 

## creating a division column and filtering to only shifts 2010 and beyond
shifts <- shifts %>% 
  mutate(assignment = str_to_lower(assignment)) %>% 
  extract(assignment, "division", "(^\\d..)", remove = F) %>% 
  filter(shift_year >= 2010)  %>% 
  mutate(shift_start_year = lubridate::year(startdate))
  

shifts %>% 
  write_csv("created_data/louisville/shifts.csv")
