## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-22
##

library(tidyverse)


# cleaning shifts data ----------------------------------------------------

sheets <- readxl::excel_sheets("raw_data/louisville/shifts_worked/work_schedules_louisville.xlsx")

shifts <- map_df(sheets, ~readxl::read_excel("raw_data/louisville/shifts_worked/work_schedules_louisville.xlsx", sheet = .) %>% 
      janitor::clean_names() %>% mutate(badge = as.character(badge)))

shifts_head <- shifts %>% 
  head(30)

shifts <- shifts %>% 
  mutate(shift_length = enddate - startdate) %>% 
  mutate(shift_length = lubridate::seconds_to_period(shift_length)) %>% 
  mutate(shift_year = lubridate::year(startdate),
         shift_month = lubridate::month(startdate),
         shift_day = lubridate::day(startdate)) 

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

shifts <- shifts %>% 
  mutate(shift_hours = lubridate::hour(shift_length),
         shift_minutes = lubridate::minute(shift_length))


shifts %>% 
  write_csv("created_data/louisville/shifts.csv")
