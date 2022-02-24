## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-23
##

library(tidyverse)
library(lubridate)



colnames <- c( "AGENCY_DESC",  "CASE_NUMBER"     ,   "CITATION_YEAR"    , "CITATION_CONTROL_N" , "CITATION_TYPE_DESC" ,   "CITATION_DATE"    , "CITATION_LOCATION"  ,      "DIVISION"      ,        "BEAT"        ,    "PERSONS_SEX"     ,    "PERSONS_RACE"    , "PERSONS_ETHNICITY"  ,    "PERSONS_AGE"     , "PERSONS_HOME_CITY"  , "PERSONS_HOME_STATE" ,  "PERSONS_HOME_ZIP"  ,   "VIOLATION_CODE"   ,     "ASCF_CODE"      ,      "STATUTE"       ,    "CHARGE_DESC"     ,      "UCR_CODE"      ,      "UCR_DESC"      ,      "emfname"       ,      "emmname"       ,      "emlname"       ,     "emdept_id"      )
uniform_citation <- read_csv("raw_data/louisville/uniform_citation/uniform_citation_louisville.txt",
                             col_names = colnames) %>% janitor::clean_names()

assaulted_officers <- readxl::read_excel("raw_data/louisville/assaulted_officers/leoka_data_louisville.xlsx") %>% 
  janitor::clean_names()


assaulted_officers_trend <- assaulted_officers %>% 
  mutate(year_occurred = year(date_occured), month_occurred = month(date_occured)) %>% 
  extract(lmpd_division, "division", "(^\\d..)") %>% 
  mutate(division = str_to_lower(division)) %>% 
  mutate(division = glue::glue("Division: {division}")) %>% 
  group_by(division, year_occurred, month_occurred) %>% 
  summarize(num_assaults = n()) %>% 
  mutate(date = lubridate::ymd(paste0(year_occurred, "-", month_occurred, "-1"))) %>% 
  filter(year_occurred < 2020) %>% 
  ggplot(aes(date, num_assaults)) +
  geom_line() +
  geom_vline(xintercept = as_date("2016-05-01"), linetype = "dashed", color = "red") +
  facet_wrap(~division) +
  theme_minimal()
        


  
uniform_citation <- uniform_citation %>% 
  separate(citation_date, c("citation_date", "citation_time"), sep = "\\s{1,}") %>% 
  mutate(citation_date = lubridate::mdy(citation_date)) %>% 
  mutate(year = lubridate::year(citation_date), month = lubridate::month(citation_date)) 


uniform_citation_trend <- uniform_citation %>% 
  group_by(year, month, division) %>% 
  summarize(count = n()) %>% 
  mutate(date = lubridate::ymd(paste0(year, "-", month, "-1"))) %>% 
  filter(year < 2020) %>% 
  filter(division %in% c(1:9) | is.na(division)) %>% 
  mutate(division = glue::glue("Division: {division}")) %>% 
  ggplot(aes(date, count)) +
  geom_line() +
  geom_vline(xintercept = as_date("2016-05-01"), linetype = "dashed", color = "red") +
  facet_wrap(~division) +
  labs(x = " ", y = "Number of Uniform Citations") +
  theme_minimal()
