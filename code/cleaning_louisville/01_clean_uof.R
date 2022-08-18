## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-08-18
##

library(tidyverse)
library(lubridate)

## obtain all use of force files from file folder
files <- list.files("raw_data/louisville/use_of_force/", pattern = "columns_mary.xlsx$")

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

## separate all of the different forces used into different columns. Number of force columns unknown here, but that's ok
uof_2017 <- uof_2017 %>% 
  mutate(force_info = str_split(force_info, "\\|")) %>% 
  unnest(force_info) %>% 
  group_by(page_start, date_of_occurrence, time_of_occurrence, officer_involved, name_badge_number) %>% 
  mutate(row = row_number()) %>% 
  mutate(row = glue::glue("force_used_{row}")) %>% 
  pivot_wider(names_from = row, values_from = force_info) %>% 
  ungroup()


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
  drop_na((name_badge_number)) %>% 
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
  mutate(force_info = str_split(force_info, "\\|")) %>% 
  unnest(force_info) %>% 
  group_by(page_start, date_of_occurrence, time_of_occurrence, officer_involved, name_badge_number) %>% 
  mutate(row = row_number()) %>% 
  mutate(row = glue::glue("force_used_{row}")) %>% 
  pivot_wider(names_from = row, values_from = force_info) %>% 
  ungroup()

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
  drop_na(name_badge_number) %>% 
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
  mutate(force_info = str_split(force_info, "\\|")) %>% 
  unnest(force_info) %>% 
  group_by(page_start, date_of_occurrence, time_of_occurrence, officer_involved, name_badge_number) %>% 
  mutate(row = row_number()) %>% 
  mutate(row = glue::glue("force_used_{row}")) %>% 
  pivot_wider(names_from = row, values_from = force_info) %>% 
  ungroup()

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
  drop_na(name_badge_number) %>% 
  mutate(time_of_occurrence = hms::as_hms(time_of_occurrence)) %>% 
  extract(name_badge_number, into = "badge_number", "(\\d{4})", remove = F) %>% 
  mutate(officer_name = str_replace(name_badge_number, "- \\d\\d\\d\\d|-\\d\\d\\d\\d", "")) %>% 
  mutate(across(where(is.character), ~str_trim(.))) %>% 
  mutate(id = paste0(page_start, "-2019-m"))


uof_mary <- bind_rows(uof_2017, uof_2018, uof_2019)

uof_mary %>% 
  colnames()

# binding uof 2014-2016 ---------------------------------------------------
## big problem: no time_of_occurrence in this data
## question: how can we know when a use-of-force instance happens within a shift?

uof_14 <- readxl::read_excel("raw_data/louisville/use_of_force/uof_14_16_mary.xlsx")

## renaming a couple columns 
uof_14 <- uof_14 %>% 
  rename(case_number = x1,
         date_received = date_receive)

## changing the dates to be actual dates as excel does somethign weird.
## counting the number of delimiters there are, and then splitting so I can separate officers
uof_14 <- uof_14 %>% 
  mutate(across(starts_with("date"), ~janitor::excel_numeric_to_date(as.numeric(.)))) %>% 
  mutate(number_officers_involved = str_count(involved_officer_s, ";") + 1,
         max_officers_involved = max(number_officers_involved)) %>% 
  separate(involved_officer_s, into = c("incident_officer_1","incident_officer_2",
                                        "incident_officer_3", "incident_officer_4",
                                        "incident_officer_5", "incident_officer_6",
                                        "incident_officer_7", "incident_officer_8",
                                        "incident_officer_9"), ";") 

## similar to above, splitting the force by a delimiter. I first count how many times the delimiter appears
uof_14 <- uof_14 %>% 
  mutate(force_officer = str_count(allegation_s_force_type_s, "\\|") + 1,
         max_force_officer = max(force_officer, na.rm = T)) %>% 
  separate(allegation_s_force_type_s, into = c("force_used_officer_1", "force_used_officer_2",
                                               "force_used_officer_3", "force_used_officer_4",
                                               "force_used_officer_5", "force_used_officer_6",
                                               "force_used_officer_7", "force_used_officer_8",
                                               "force_used_officer_9"),
           sep = "\\|") 

## pivoting into longer format fro the incident officer and force used columns
## syncing up columns with uof_mary
## dropping any NAs from officers
## separating out force_used by delimiters
## changing everything to lowercase and trimming the strings
uof_14 <- uof_14 %>% 
  pivot_longer(matches("^incident_officer|^force_used_officer"), names_to = c(".value", "officer_involved"), names_pattern = "(.+)_(\\d)") %>% 
  mutate(officer_involved = paste0("incident_officer_", officer_involved)) %>% 
  drop_na(incident_officer) %>% 
  extract(incident_officer, into = "badge_number", "(\\d{4})", remove = F) %>%
  extract(incident_officer, into = "officer_name", "(.+)Police|Corrections|Fire.+", remove = F) %>% 
  separate(force_used_officer, into = c("force_used_1", "force_used_2",
                                        "force_used_3", "force_used_4", 
                                        "force_used_5", "force_used_6",
                                        "force_used_7", "force_used_8",
                                        "force_used_9"), 
           sep = ";") %>% 
  mutate(across(where(is.character), ~str_trim(.) %>% str_to_lower())) 

## syncing with uof_mary.
## renaming, and selecting only things that are relevant to both data sets.
uof_14 <- uof_14 %>% 
  select(-starts_with("max"), -force_officer, -number_officers_involved, -date_received, - completed_d,
         -incident_type) %>% 
  rename(name_badge_number = incident_officer,
         notes = summary,
         id = case_number,
         date_of_occurrence = date_occurred) 


# merging uof_mary and the uof_14_16 --------------------------------------

uof_cleaned <- uof_mary %>% 
  mutate(unit_district = NA_character_,
         department = NA_character_) %>% 
  bind_rows(uof_14) 



uof_cleaned %>% 
  write_csv("created_data/louisville/uof_cleaned.csv")
