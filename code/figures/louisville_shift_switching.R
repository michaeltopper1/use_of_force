## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-05
##

library(tidyverse)
library(lubridate)
library(patchwork)

shifts <- read_csv("created_data/louisville/shifts.csv") %>% 
  filter(rank == "POLICE OFFICER") %>% 
  filter(shift_hours %in% c(8,10,12))

badges <- shifts %>% 
  distinct(badge) 

badges_8 <- badges %>% 
  mutate(shift_hours = 8)
badges_10 <- badges %>% 
  mutate(shift_hours = 10)
badges_12 <- badges %>% 
  mutate(shift_hours = 12)

badges_all <- bind_rows(badges_8, badges_10, badges_12)


post_change <- shifts %>% 
  filter(start_date >= as_date("2016-05-16")) %>% 
  group_by(badge) %>% 
  count(shift_hours, name = "count_post_change")

mid_change <- shifts %>% 
  filter(start_date > as_date("2015-10-12") & start_date < as_date("2016-05-16")) %>% 
  group_by(badge) %>% 
  count(shift_hours, name = "count_middle_change") 

pre_change <- shifts %>% 
  filter(start_date <= as_date("2015-10-12")) %>% 
  group_by(badge) %>% 
  count(shift_hours, name = "count_pre_change") 

badges_all <- badges_all %>% 
  left_join(post_change) %>% 
  left_join(mid_change) %>% 
  left_join(pre_change) %>% 
  mutate(across(ends_with("change"), ~ifelse(is.na(.), 0, .)))

badges_all <- badges_all %>% 
  group_by(badge) %>% 
  mutate(total_shifts_post = sum(count_post_change),
         total_shifts_middle = sum(count_middle_change),
         total_shifts_pre =  sum(count_pre_change)) %>% 
  arrange(badge) %>% 
  rowwise() %>% 
  mutate(fraction_pre = count_pre_change/total_shifts_pre,
         fraction_mid = count_middle_change/total_shifts_middle,
         fraction_post = count_post_change/total_shifts_post) %>% 
  mutate(across(starts_with("fraction"), ~ifelse(is.nan(.), 0, .))) %>% 
  ungroup()

pre_dist <- badges_all %>% 
  mutate(shift_hours = glue::glue("{shift_hours} Hour Shift")) %>% 
  mutate(shift_hours = factor(shift_hours, levels = c("8 Hour Shift","10 Hour Shift","12 Hour Shift"))) %>% 
  ggplot(aes(fraction_pre)) +
  geom_histogram(bins = 10) +
  facet_wrap(~shift_hours) +
  labs(x = "Fraction of Shifts by Officer", y = "Count", title = "Panel A: Pre Change") +
  theme_minimal()

mid_dist <- badges_all %>% 
  mutate(shift_hours = glue::glue("{shift_hours} Hour Shift")) %>% 
  mutate(shift_hours = factor(shift_hours, levels = c("8 Hour Shift","10 Hour Shift","12 Hour Shift"))) %>% 
  ggplot(aes(fraction_mid)) +
  geom_histogram(bins = 10) +
  facet_wrap(~shift_hours) +
  labs(x = "Fraction of Shifts by Officer", y = "Count", title = "Panel B: Mid Change") +
  theme_minimal()

post_dist <- badges_all %>% 
  mutate(shift_hours = glue::glue("{shift_hours} Hour Shift")) %>% 
  mutate(shift_hours = factor(shift_hours, levels = c("8 Hour Shift","10 Hour Shift","12 Hour Shift"))) %>% 
  ggplot(aes(fraction_post)) +
  geom_histogram(bins = 10) +
  facet_wrap(~shift_hours) +
  labs(x = "Fraction of Shifts by Officer", y = "Count", title = "Panel C: Post Change") +
  theme_minimal()

pre_dist + mid_dist + post_dist + plot_layout(nrow = 3)
