## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-08-13
##

library(tidyverse)


## getting the datetime of the uof
uof <- read_csv("created_data/louisville/uof_mary.csv") %>% 
  mutate(date_of_occurrence = as_date(date_of_occurrence),
         datetime_of_occurence = as_datetime(paste(date_of_occurrence, time_of_occurrence))) %>% 
  mutate(unique_id = row_number())


shifts <- read_csv("created_data/louisville/shifts_demographics.csv")

test <- shifts %>% 
  mutate(unique_id = row_number()) %>% 
  inner_join(uof_light, by = c("badge" = "badge_number"))

test_sub <- test %>% 
  head(1000)

test_merge <- test %>% 
  rowwise %>% 
  mutate(match = ifelse(between(datetime_of_occurence, start_date, end_date), 1 , NA)) %>% 
  distinct(unique_id, .keep_all = T)


uof_light <- uof %>% 
  select(datetime_of_occurence, badge_number) %>% 
  distinct()



