## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-09-28
##

library(tidyverse)
library(lubridate)

## remember that termination date for experience not just your date. fix at some point
shifts <- read_csv("created_data/louisville/shifts_officers_cleaned.csv") %>% 
  select(-shift_hours, -start_date, -end_date, -officer_experience) %>% 
  rename(shift_id = unique_id)

officer_demographics <- read_csv("created_data/louisville/officer_demographics_cleaned.csv")

uof <- read_csv("created_data/louisville/uof_cleaned.csv") 
  


# attempting to merge at daily level --------------------------------------
## creates a 91% match
## i do a weekly merge later on. 

dirty_merge <- shifts %>% 
  left_join(uof, by = c("badge" = "badge_number"))

merged_uof_daily <- dirty_merge %>% 
  mutate(shift_interval = interval(startdate, enddate + hours(4))) %>% 
  mutate(occurrence = ifelse(date_time_uof_occurrence %within% shift_interval, 1, 0)) %>% 
  filter(occurrence == 1) 

## an officer can have multiple use of force reports within a single shift which is why
## there are doubles here in merged_uof. This is totally ok, albeit rare.

shifts_with_uof <- merged_uof_daily %>% 
  distinct(shift_id) %>% 
  pull(shift_id)

## getting the shifts that do not have a use of force report attached to it at daily level
shifts_no_uof_daily <- shifts %>% 
  filter(!shift_id %in%shifts_with_uof)


merged_uof_ids_daily <- merged_uof_daily %>% 
  pull(uof_id)

## writing the unmerged uof data at the daily level to a csv
uof %>% 
  filter(!uof_id %in% merged_uof_ids_daily) %>% 
  left_join(officer_demographics, by = c("badge_number" = "badge")) %>% 
  write_csv("created_data/louisville/unmerged_uof_daily.csv")

shifts_uof_merge_daily <- merged_uof_daily %>% 
  bind_rows(shifts_no_uof_daily)



# getting rid of bad data -------------------------------------------------

## getting rid of shifts that are over 23 hours long, or shifts under 1 hour
shifts_uof_merge_daily <- shifts_uof_merge_daily %>% 
  filter(shift_length <= 24 & shift_length > 0) 


## creating helpful columns
shifts_uof_merge_daily <- shifts_uof_merge_daily %>% 
  mutate(shift_length = as.double(enddate - startdate, units = "hours")) %>% 
  mutate(shift_length = round(shift_length, 0)) %>% 
  mutate(shift_uof = ifelse(!is.na(uof_id), 1, 0)) %>% 
  mutate(startdate_asdate = as_date(startdate)) %>% 
  mutate(shift_ett = case_when(
    shift_length == 8 ~ "8 Hour",
    shift_length == 10 ~"10 Hour",
    shift_length == 12 ~"12 Hour",
    TRUE ~ "Other"
  )) %>% 
  mutate(date_year_month = ymd(paste0(year(startdate_asdate), "-",month(startdate_asdate), "-1")),
         .before = 1) 

## creating age at shift
shifts_uof_merge_daily <- shifts_uof_merge_daily %>% 
  mutate(year_minus = case_when(
    shift_year == 2020 ~0,
    shift_year == 2019 ~1,
    shift_year == 2018 ~2,
    shift_year == 2017 ~3, 
    shift_year == 2016 ~4,
    shift_year == 2015 ~5,
    shift_year == 2014 ~6,
    shift_year == 2013 ~7,
  )) %>% 
  rowwise() %>% 
  mutate(age_at_shift = officer_age - year_minus,
         experience_at_shift = officer_years_sworn - year_minus) %>% 
  select(-year_minus) %>% 
  ungroup()

shifts_uof_merge_daily %>% 
  write_csv("created_data/louisville/shifts_uof_merged_daily.csv")

# mergining at the weekly level -------------------------------------------

## going to aggregate the uof data by officer to the week-year level
uof_weekly <- uof %>% 
  mutate(year = year(date_of_occurrence),
         week = week(date_of_occurrence))
uof_weekly %>% 
  group_by(badge_number, year, week) %>% 
  mutate(number_uof_incidents_week = n(), .before = 1) %>% 
  arrange(desc(number_uof_incidents_week)) %>% View()

shifts %>% 
  colnames()


