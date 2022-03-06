## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-26
##

library(tidyverse)
library(lubridate)

colnames <- c( "AGENCY_DESC",  "CASE_NUMBER"     ,   "CITATION_YEAR"    , "CITATION_CONTROL_N" , "CITATION_TYPE_DESC" ,   "CITATION_DATE"    , "CITATION_LOCATION"  ,      "DIVISION"      ,        "BEAT"        ,    "PERSONS_SEX"     ,    "PERSONS_RACE"    , "PERSONS_ETHNICITY"  ,    "PERSONS_AGE"     , "PERSONS_HOME_CITY"  , "PERSONS_HOME_STATE" ,  "PERSONS_HOME_ZIP"  ,   "VIOLATION_CODE"   ,     "ASCF_CODE"      ,      "STATUTE"       ,    "CHARGE_DESC"     ,      "UCR_CODE"      ,      "UCR_DESC"      ,      "emfname"       ,      "emmname"       ,      "emlname"       ,     "emdept_id"      )
uniform_citation <- read_csv("raw_data/louisville/uniform_citation/uniform_citation_louisville.txt",
                             col_names = colnames) %>% janitor::clean_names() %>% 
  mutate(across(ends_with("name"), ~str_to_lower(.))) %>% 
  mutate(across(ends_with("name"), ~str_replace_all(.,"`|'", ""))) %>% 
  mutate(full_name = paste(emfname, emlname, emdept_id))
shifts <- read_csv("created_data/louisville/shifts.csv") %>% 
  mutate(across(ends_with("name"), ~str_to_lower(.))) %>% 
  mutate(across(ends_with("name"), ~str_replace_all(.,"`|'", ""))) %>% 
  mutate(full_name = paste(first_name, last_name, badge))


uniform_citation <- uniform_citation %>% 
  mutate(citation_date_hms = mdy_hms(citation_date)) %>% 
  mutate(citation_date = as_date(citation_date_hms)) 
  
daryl <- uniform_citation %>% 
  filter(full_name == "daryl neese 7754" )

daryl_shifts <- shifts %>% 
  filter(full_name == "daryl neese 7754")

daryl_shifts <- daryl_shifts %>% 
  mutate(shift_interval = interval(start_date, end_date), .before = 1)


daryl_counts <- daryl %>% 
  mutate(citation_hour = hour(citation_date_hms), .before = 1) %>% 
  group_by(citation_date, citation_hour) %>% 
  summarize(citations = n()) 

daryl_start <- daryl_shifts %>% 
  mutate(start_date_d = as_date(start_date), end_date_d = as_date(end_date)) %>% 
  left_join(daryl_counts, by = c("start_date_d" = "citation_date")) %>% 
  relocate(citations, citation_hour) %>% 
  mutate(date_hm = ymd_hm(paste0(start_date_d," ", citation_hour, ":00")), .before = 1) %>% 
  mutate(citations_in_interval = ifelse(date_hm %within% shift_interval, citations, 0), .before = 1) %>% 
  group_by(shift_interval) %>% 
  summarize(citations_start_total = sum(citations_in_interval))

## look at this link for merge:
## https://stackoverflow.com/questions/69288212/left-join-subset-of-column-based-on-date-interval

## As of right now, there are a lot of arrests taht do not happen within a shift period.
## for instance, there are some that happen on monday at 4:45 when the shift ends at 2:00. 
## the merge cannot pick this up, nor can anything. Would have to aggregate to weekly level as robustness.
## due to this issue, i am not finishing the merge. 
daryl_shifts %>% 
  mutate(start_date_d = as_date(start_date), end_date_d = as_date(end_date)) %>%  
  full_join(daryl_counts, by = c("end_date_d" = "citation_date")) %>% 
  relocate(citations, citation_hour) %>% 
  mutate(date_hm = ymd_hm(paste0(start_date_d," ", citation_hour, ":00")), .before = 1) %>% 
  mutate(citations_in_interval = ifelse(date_hm %within% shift_interval, citations, 0), .before = 1) %>% 
  group_by(shift_interval) %>% 
  summarize(citations_start_total = sum(citations_in_interval)) %>% 
  summarize(total = sum(citations_start_total, na.rm = T))

daryl_end <- daryl_shifts %>% 
  mutate(start_date_d = as_date(start_date), end_date_d = as_date(end_date)) %>%  
  left_join(daryl_counts, by = c("end_date_d" = "citation_date")) %>% 
  relocate(citations, citation_hour) %>% 
  mutate(date_hm = ymd_hm(paste0(start_date_d," ", citation_hour, ":00")), .before = 1) %>% 
  mutate(citations_in_interval = ifelse(date_hm %within% shift_interval, citations, 0), .before = 1) %>% 
  group_by(shift_interval) %>% 
  summarize(citations_end_total = sum(citations_in_interval)) 


daryl_total <- daryl_start %>% 
  left_join(daryl_end)

daryl_total <- daryl_total %>% 
  rowwise() %>% 
  mutate(citations = sum(citations_start_total, citations_end_total, na.rm = T))


daryl_counts <- daryl %>% 
  mutate(citation_hour = hour(citation_date_hms), .before = 1) %>% 
  group_by(citation_date, citation_hour) %>% 
  summarize(citations = n()) 
