## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-08-19
##

library(tidyverse)
library(lubridate)


shifts <- read_csv("created_data/louisville/shifts_officers_cleaned.csv")


missing_rank <- shifts %>% 
  filter(is.na(rank_title)) %>% nrow()


shifts %>% 
  filter(rank == "police officer") %>% 
  filter(shift_hours %in% c(8, 10, 12)) %>% 
  group_by(date_bins, badge) %>% 
  mutate(total_shifts = n(), .before = 1) %>% 
  ungroup() %>% 
  group_by(date_bins, badge, shift_hours, total_shifts) %>% 
  summarize(n = n()) %>% 
  rowwise() %>% 
  mutate(proportion = n/total_shifts) %>% 
  ungroup() %>% 
  ggplot(aes(proportion)) +
  geom_histogram() +
  facet_wrap(~date_bins) +
  theme_minimal()
