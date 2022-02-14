## Purpose of script: append the 911 data calls november 2017 to present
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-13
##

library(tidyverse)

files_911 <- list.files("raw_data/chicago/911_calls/cpd_foia214161/")

calls <- map_df(list(files_911), ~read_csv(paste0("raw_data/chicago/911_calls/cpd_foia214161/", .)))

calls <- calls %>% 
  janitor::clean_names() %>% 
  mutate(entry_date = lubridate::mdy_hms(entry_date), 
         entry_type = lubridate::mdy_hms(event_type))

calls %>% 
  write_csv("raw_data/chicago/911_calls/911_calls_appended.csv")
