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


shift_fractions <- shifts %>% 
  filter(shift_start_year < 2020) %>% 
  filter(rank == "POLICE OFFICER") %>% 
  filter(shift_hours %in% c(8,10,12)) %>% 
  count(division) %>% 
  rowwise() %>% 
  mutate(fraction_shifts = n/number_rows) %>% 
  mutate(division = ifelse(is.na(division), "No Division Assignment", division)) %>% 
  janitor::adorn_totals() %>% 
  kbl(booktabs = T, col.names = c("Division", "Shifts", "Fraction of Total Shifts"),
      digits = 3, format = "latex") %>% 
  kable_paper() %>% 
  row_spec(10, hline_after = T)
  footnote(list("Sample includes only police officers (e.g., no lieutenants/sergeants/detectives) that worked in either 8, 10, or 12 hour shifts in the years 2010-2019"))

shifts %>% 
  filter(rank == "POLICE OFFICER") %>% 
  filter(shift_hours %in% c(8,10,12)) %>% 
  mutate(shift_hours = glue::glue("{shift_hours}-Hour Shift")) %>% 
  modelsummary::datasummary((`High School` = high_school_grad) + ( `Some College` = some_college) + 
                              (`College` = college) + (`Graduate Degree` = graduate_degree) + 
                              (`Unknown Education` = unknown_schooling) + (`Officer Tenure` = officer_tenure) ~ shift_hours * (Mean + SD) *Arguments(na.rm = TRUE),
                            data = ., output = "latex")


