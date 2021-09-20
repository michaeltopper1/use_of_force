## Purpose of script: rough code that finds the amount switching of officers
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-19
##

library(tidyverse)

library(tidyverse)
library(readxl)

officers <- read_excel("raw_data/police_demographics/demographics.xlsx") %>% 
  janitor::clean_names()
shifts <- read_excel("raw_data/shifts/work_schedules.xlsx", sheet = "2016") %>% 
  janitor::clean_names()



## join together the two data sets
both <- shifts %>% 
  left_join(officers)

## split into a list of data frames and assign the assignment the name of the list element
list_of_people <- both %>% 
  group_split(assignment) %>% 
  setNames(unique(both$assignment))

## finds the unique badges between each assignment
unique_badge_shifts <- map(list_of_people, ~.x %>% distinct(badge)) 

##gets the unique badges for the entire 2016 data
unique_badges <- both %>% 
  distinct(badge) %>% 
  pull()

## finds the amount of switching 
unique_badge_shifts %>% 
  unlist() %>% as_tibble() %>% 
  count(value, sort = T) %>% filter(n >2)
