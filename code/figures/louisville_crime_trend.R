## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-25
##

library(tidyverse)
library(lubridate)


crime <- read_csv("raw_data/louisville/crime/crime_2010_2019.csv")

crime %>% 
  distinct(year_reported, incident_number, year_occurred, month_occurred, month_reported, lmpd_division) %>% 
  mutate(date = ymd(paste0(year_occurred, "-", month_occurred, "-1"))) %>% 
  group_by(date, lmpd_division) %>% 
  summarize(count  = n()) %>% 
  filter(date >= "2010-10-01") %>%
  ggplot(aes(date, count)) +
  geom_point() +
  geom_vline(xintercept = as_date("2016-05-01"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as_date("2015-09-01"), linetype = "dashed", color = "red") +
  facet_wrap(~lmpd_division) +
  labs(x = " ", y = "Number of Crimes") +
  theme_minimal() +
  theme(legend.position = "bottom")
