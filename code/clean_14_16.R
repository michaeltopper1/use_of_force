## Purpose of script: extract the data from pdf 2014-2016 on use of force
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-20
##

library(tidyverse)
library(tabulizer)
library(pdftools)

## reading in the data
uof_14_16 <- readxl::read_excel("raw_data/use_of_force/uof_2014_2016.xlsx") %>% 
  janitor::clean_names()

## getting rid of the column that does not have any data in it
uof_14_16 <- uof_14_16 %>% 
  select(-x2, -x3, -x8) %>% 
  rename(incident_id = x1,
         date_received = date_receive) 


## there are up to 9 officers in this data. I extract each of the badge numbers and put into separate columns
## column 10 should be empty
uof_14_16 <- uof_14_16 %>% 
  separate(involved_officer_s, c("officer_1", "officer_2", "officer_3", "officer_4", "officer_5", "officer_6",
                                 "officer_7", "officer_8", "officer_9", "officer_10"),
           sep = "]", extra = "merge", remove = F) %>% 
  extract(officer_1, "officer_1", "(\\d{4})") %>% 
  extract(officer_2, "officer_2", "(\\d{4})") %>% 
  extract(officer_3, "officer_3", "(\\d{4})") %>% 
  extract(officer_4, "officer_4", "(\\d{4})") %>% 
  extract(officer_5, "officer_5", "(\\d{4})") %>% 
  extract(officer_6, "officer_6", "(\\d{4})") %>% 
  extract(officer_7, "officer_7", "(\\d{4})") %>% 
  extract(officer_8, "officer_8", "(\\d{4})") %>% 
  extract(officer_9, "officer_9", "(\\d{4})") %>% 
  extract(officer_10, "officer_10", "(\\d{4})") 

## here I pivoted to long format so that there are duplicate incident ids for each officer involved.
uof_14_16 <- uof_14_16 %>% 
  pivot_longer(starts_with("officer"), values_to = "officer_badge") %>% 
  select(-name) %>% 
  filter(!is.na(officer_badge)) %>% 
  rename(involved_officers = involved_officer_s,
         allegations_force_type = allegation_s_force_type_s,
         completed = completed_d)

## saving to csv
write_csv(uof_14_16, file = "created_data/uof_16.csv")

