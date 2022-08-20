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

## looks like i put the floor_date to hours. Shifts start on the hour and end on the hour
## start_date is the floor date of startdate. For instance, 8:30pm will be 8:00pm 
shifts <- shifts %>% 
  mutate(start_date = floor_date(startdate, unit = "hours"),
         end_date = floor_date(enddate, unit = "hours"),
         shift_length = seconds(end_date - start_date)) %>% 
  mutate(shift_length = seconds_to_period(shift_length), .before = 1) %>% 
  mutate(shift_hours = hour(shift_length), .before =1 ) %>% 
  mutate(shift_year = lubridate::year(start_date),
         shift_month = lubridate::month(start_date),
         shift_day = lubridate::day(start_date))  %>% 
  mutate(start_hour = hour(startdate), end_hour = hour(enddate)) %>% 
  filter(shift_year >= 2010)  


## getting rid of duplicates
shifts <- shifts %>% 
  distinct() 


# merging with shifts data ------------------------------------------------

officers <- read_csv("created_data/louisville/officer_demographics_cleaned.csv") %>% 
  mutate(badge = as.character(badge))

## successfully merge with no duplications
shifts_officers <- shifts %>% 
  left_join(officers, by = c("badge"))


shifts_officers <- shifts_officers %>% 
  mutate(across(where(is.character), ~str_trim(.) %>% str_to_lower())) %>% 
  mutate(switch_date_all_8 = as_date("2015-09-01"),
         switch_date_12 = as_date("2016-05-01")) %>% 
  mutate(date_bins = case_when(
    start_date < switch_date_all_8 ~ "pre",
    start_date >= switch_date_all_8 & start_date < switch_date_12 ~ "mid",
    start_date >= switch_date_12 ~ "post"
  ))


  
## saving as csv
shifts_officers %>% 
  write_csv(file = here::here("created_data/louisville/shifts_officers_cleaned.csv"))
