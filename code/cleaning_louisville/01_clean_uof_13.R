## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-09-20
##

library(tidyverse)

uof_13 <- read_csv("raw_data/louisville/use_of_force/uof_13_16_lawyers.csv")

uof_13 <- uof_13 |> 
  rename_with(~str_replace(.x, pattern = "^Inc:", replacement = ""), matches("^Inc:")) |> 
  rename_with(~str_replace(.x, pattern = "\\(.+", ""), matches(".+")) |> 
  janitor::clean_names()

uof_13 |> 
  colnames()
## it appears that the mary CSV and this CSV are very similar. Only a difference of 6 uof incidents which is a good sign

uof_13 <- uof_13 |> 
  mutate(across(ends_with("date"), ~lubridate::mdy(.))) |> 
  mutate(year_occurred = lubridate::year(occurred_date)) 

unique_id <- uof_13 |> 
  distinct(database_row_number) |> 
  pull()

uof_13 |> 
  distinct(year_occurred, database_row_number) |> 
  count(year_occurred)
