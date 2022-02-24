## Purpose of script: This figure shows the number of 8/10/12 hour shifts fro POLICE OFFICERS ONLY in LOUSIVILLE over the sample period
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-23
##

library(tidyverse)
library(lubridate)

shifts <- read_csv("created_data/louisville/shifts.csv")

police_officer_shifts <- shifts %>% 
  filter(rank == "POLICE OFFICER")

police_officer_shifts <- police_officer_shifts %>% 
  mutate(date = lubridate::ymd(paste0(shift_year, "-", shift_month, "-1")))

police_officer_shifts_figure <- police_officer_shifts %>% 
  filter(shift_year >= 2010) %>% 
  filter(shift_hours %in% c(8,10,12)) %>% 
  group_by(date) %>% 
  count(shift_hours, sort = T) %>% 
  arrange(date) %>% 
  ggplot(aes(date, n, color = as.factor(shift_hours), label = shift_hours, linetype = as.factor(shift_hours))) +
  geom_path() +
  geom_point() + 
  labs(x = " ", y = "Number of Shifts", color = "Hours in Shift", linetype = "Hours in Shift") +
  theme_minimal() +
  theme(legend.position = "bottom")

police_officer_shifts_trends_division <- police_officer_shifts %>% 
  mutate(assignment = str_to_lower(assignment)) %>% 
  extract(assignment, "division", "(^\\d..)") %>% 
  filter(shift_year >= 2010) %>% 
  filter(shift_hours %in% c(8,10,12)) %>% 
  mutate(shift_hours = as.factor(shift_hours)) %>% 
  mutate(division = glue::glue("Division: {division}")) %>% 
  group_by(date, division) %>% 
  count(shift_hours, sort = T) %>% 
  arrange(date) %>% 
  ggplot(aes(date, n, color = shift_hours, label = shift_hours, linetype = shift_hours, shape = shift_hours)) +
  geom_point() + 
  geom_vline(xintercept = as_date("2016-05-01"), linetype = "dotted", color = "black") +
  facet_wrap(~division, scales = "free_y") +
  labs(x = " ", y = "Number of Shifts", color = "Hours in Shift", linetype = "Hours in Shift", shape = "Hours in Shift") +
  theme_minimal() +
  theme(legend.position = "bottom")
