## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-24
##

library(tidyverse)
library(patchwork)
library(modelsummary)
library(kableExtra)
theme_set(theme_minimal())


shifts <- read_csv("created_data/louisville/shifts.csv")

number_rows <- shifts %>% filter(rank == "POLICE OFFICER") %>% filter(shift_hours %in% c(8,10,12)) %>% 
  filter(shift_start_year < 2020) %>% nrow


shifts %>% 
  filter(shift_start_year < 2020) %>% 
  filter(rank == "POLICE OFFICER") %>% 
  filter(shift_hours %in% c(8,10,12)) %>% 
  count(division) %>% 
  rowwise() %>% 
  mutate(fraction_shifts = n/number_rows) %>% 
  mutate(division = ifelse(is.na(division), "No Division Assignment", division)) %>% 
  janitor::adorn_totals()

shifts %>% 
  filter(rank == "POLICE OFFICER") %>% 
  filter(shift_hours %in% c(8,10,12)) %>% 
  mutate(shift_hours = glue::glue("{shift_hours}-Hour Shift")) %>% 
  modelsummary::datasummary(high_school_grad + some_college + 
                              college + graduate_degree + 
                              unknown_schooling + officer_tenure ~ shift_hours * (Mean + SD) *Arguments(na.rm = TRUE),
                            data = .)


