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
  mutate(shift_interval = interval(startdate, enddate), .before = 1)

daryl_shifts %>% 
  mutate(start_date = as_date(startdate), end_date = as_date(enddate)) %>% 
  left_join(daryl_counts, by = c("start_date" = "citation_date")) %>% 
  relocate(citations, citation_hour) %>% 
  mutate(date_hm = ymd_hm(paste0(start_date," ", citation_hour, ":00")), .before = 1) %>% 
  mutate(citations_in_interval = ifelse(date_hm %within% shift_interval, citations, 0), .before = 1) %>% View()
  group_by(shift_interval) %>% 
  summarize(citations_start_total = )
daryl_counts <- daryl %>% 
  mutate(citation_hour = hour(citation_date_hms), .before = 1) %>% 
  group_by(citation_date, citation_hour) %>% 
  summarize(citations = n()) 
