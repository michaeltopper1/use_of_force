## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-04-18
##

library(tidyverse)
library(lubridate)

## obtain all use of force files from file folder
files <- list.files("raw_data/louisville/use_of_force/")

## read in all uof files
uof <- map(files, ~readxl::read_excel(paste0("raw_data/louisville/use_of_force/", .)) %>% 
      janitor::clean_names())

## rename all uof files to get rid of the formatting help that I gave to the data inputters
uof <- map(uof, ~.x %>% 
      rename_with(.fn = ~ str_replace(pattern = "_force_used_force_effective.{1,}",replacement =  "", .)))



# use of force 2017 cleaning ----------------------------------------------

## pivot the columns to long format 
uof_2017 <- uof %>% 
  pluck(1) %>% 
  pivot_longer(cols = starts_with("incident_officer"), names_to = c("officer_involved", "type"), names_pattern = "(incident_officer_\\d)_(.{1,})", values_to = "values") %>% 
  pivot_wider( names_from = "type", values_from = "values")


## separate all of the different forces used into 8 maximum types of force - 8 seems to be the max here
uof_2017 <- uof_2017 %>%
  separate(force_info, into = c("force_used_1", "force_used_2",
                                "force_used_3", "force_used_4", 
                                "force_used_5", "force_used_6", 
                                "force_used_7", "force_used_8"), "\\|", extra = "merge") 

## split the force columns into force_used and force_used_effective for each of the cases
## this selects the force columns and "loops" through them while created a new effective column
## need to paste this back together with the original df
force_columns_2017 <- uof_2017 %>% 
  select(starts_with("force")) %>% 
  names() %>% 
  map_dfc(function(x) 
    uof_2017 %>% 
        select(x) %>% 
        separate(x, into = c(x,paste0(x, "_effective")), sep = ";")) 

## pasting together the force columns with the original data frame. I delete the force_used columns
## to remove duplicates
uof_2017 <- uof_2017 %>% 
  select(-starts_with("force_used")) %>% 
  bind_cols(force_columns_2017)

## filters out NAs for incident officers
## changes the time to an hms since excel did a weird thing
## extracts the badge number and the officer name separately
## trims whitespace for the columns
uof_2017 <- uof_2017 %>% 
  filter(!is.na(name_badge_number)) %>% 
  mutate(time_of_occurrence = hms::as_hms(time_of_occurrence)) %>% 
  extract(name_badge_number, into = "badge_number", "(\\d{4})", remove = F) %>% 
  mutate(officer_name = str_replace(name_badge_number, "- \\d\\d\\d\\d|-\\d\\d\\d\\d", "")) %>% 
  mutate(across(where(is.character), ~str_trim(.))) %>% 
  mutate(id = paste0(page_start, "-2017-m"))


# use of force 2018 cleaning ----------------------------------------------

## pivot the columns to long format 
uof_2018 <- uof %>% 
  pluck(2) %>% 
  pivot_longer(cols = starts_with("incident_officer"), names_to = c("officer_involved", "type"), names_pattern = "(incident_officer_\\d{1,2})_(.{1,})", values_to = "values") %>%
  pivot_wider( names_from = "type", values_from = "values") 

## separate all of the different forces used into 8 maximum types of force - 8 seems to be the max here
uof_2018 <- uof_2018 %>%
  separate(force_info, into = c("force_used_1", "force_used_2",
                                "force_used_3", "force_used_4", 
                                "force_used_5", "force_used_6", 
                                "force_used_7", "force_used_8"), "\\|", extra = "merge") 

## split the force columns into force_used and force_used_effective for each of the cases
## this selects the force columns and "loops" through them while created a new effective column
## need to paste this back together with the original df
force_columns_2018 <- uof_2018 %>% 
  select(starts_with("force")) %>% 
  names() %>% 
  map_dfc(function(x) 
    uof_2018 %>% 
      select(x) %>% 
      separate(x, into = c(x,paste0(x, "_effective")), sep = ";")) 

## pasting together the force columns with the original data frame. I delete the force_used columns
## to remove duplicates
uof_2018 <- uof_2018 %>% 
  select(-starts_with("force_used")) %>% 
  bind_cols(force_columns_2018)

## filters out NAs for incident officers
## changes the time to an hms since excel did a weird thing
## extracts the badge number and the officer name separately
## trims whitespace for the columns
uof_2018 <- uof_2018 %>% 
  filter(!is.na(name_badge_number)) %>% 
  mutate(time_of_occurrence = hms::as_hms(time_of_occurrence)) %>% 
  extract(name_badge_number, into = "badge_number", "(\\d{4})", remove = F) %>% 
  mutate(officer_name = str_replace(name_badge_number, "- \\d\\d\\d\\d|-\\d\\d\\d\\d", "")) %>% 
  mutate(across(where(is.character), ~str_trim(.))) %>% 
  mutate(id = paste0(page_start, "-2018-m"))


# use of force 2019 cleaning ----------------------------------------------


## pivot the columns to long format 
uof_2019 <- uof %>% 
  pluck(3) %>% 
  pivot_longer(cols = starts_with("incident_officer"), names_to = c("officer_involved", "type"), names_pattern = "(incident_officer_\\d{1,2})_(.{1,})", values_to = "values") %>%
  pivot_wider( names_from = "type", values_from = "values") 

## separate all of the different forces used into 8 maximum types of force - 8 seems to be the max here
uof_2019 <- uof_2019 %>%
  separate(force_info, into = c("force_used_1", "force_used_2",
                                "force_used_3", "force_used_4", 
                                "force_used_5", "force_used_6", 
                                "force_used_7", "force_used_8"), "\\|", extra = "merge") 

## split the force columns into force_used and force_used_effective for each of the cases
## this selects the force columns and "loops" through them while created a new effective column
## need to paste this back together with the original df
# force_columns_2019 <- 
force_columns_2019 <- uof_2019 %>% 
  select(starts_with("force")) %>% 
  names() %>% 
  map_dfc(function(x) 
    uof_2019 %>% 
      select(x) %>% 
      separate(x, into = c(x,paste0(x, "_effective")), sep = ";")) 


## pasting together the force columns with the original data frame. I delete the force_used columns
## to remove duplicates
uof_2019 <- uof_2019 %>% 
  select(-starts_with("force_used")) %>% 
  bind_cols(force_columns_2019)

## filters out NAs for incident officers
## changes the time to an hms since excel did a weird thing
## extracts the badge number and the officer name separately
## trims whitespace for the columns
uof_2019 <- uof_2019 %>% 
  filter(!is.na(name_badge_number)) %>% 
  mutate(time_of_occurrence = hms::as_hms(time_of_occurrence)) %>% 
  extract(name_badge_number, into = "badge_number", "(\\d{4})", remove = F) %>% 
  mutate(officer_name = str_replace(name_badge_number, "- \\d\\d\\d\\d|-\\d\\d\\d\\d", "")) %>% 
  mutate(across(where(is.character), ~str_trim(.))) %>% 
  mutate(id = paste0(page_start, "-2019-m"))


uof_mary <- bind_rows(uof_2017, uof_2018, uof_2019)

uof_mary %>% 
  write_csv("created_data/louisville/uof_mary.csv")
