## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-09-27
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

# binding uof 2013-2016 ---------------------------------------------------
## question: how can we know when a use-of-force instance happens within a shift?

uof_13 <- read_csv("raw_data/louisville/use_of_force/uof_13_16_lawyers.csv")

## cleaning names
uof_13 <- uof_13 |> 
  rename_with(~str_replace(.x, pattern = "^Inc:", replacement = ""), matches("^Inc:")) |> 
  rename_with(~str_replace(.x, pattern = "\\(.+", ""), matches(".+")) |> 
  janitor::clean_names()

uof_13 <- uof_13 |> 
  rename("date_of_occurrence" = occurred_date,
         "time_of_occurrence" = occurred_time,
         "reason_for_force" = uof_reason_for_using_force,
         "badge_number" = off_employee_code_number,
         "force_used" = uof_type_of_force_used,
         "notes" = narrative,
         "citizen_age" = cit_snp_age,
         "citizen_race" = cit_race,
         "citizen_injured" = uof_citizen_was_injured)  

## cleaning
uof_13 <- uof_13 |> 
  mutate(across(where(is.character), ~str_to_lower(.))) |> 
  mutate(date_of_occurrence = lubridate::mdy(date_of_occurrence))

## removing duplicates
uof_13 <- uof_13 |> 
  select(-starts_with("uof")) |> 
  distinct() 

## widening the data so we have force types in each column  unique to each officer.
uof_13 <- uof_13 |> 
  group_by(across(-force_used)) |> 
  mutate(collapse_force = paste(force_used,  collapse = " | "),
         force_officer = str_count(collapse_force, "\\|") + 1,
         max_force_officer = max(force_officer, na.rm = T)) |> 
  separate(collapse_force, into = c("force_used_1", "force_used_2",
                                    "force_used_3", "force_used_4",
                                    "force_used_5", "force_used_6",
                                    "force_used_7"), sep = "\\|") |> 
  select(-force_used) |> 
  distinct() |> 
  ungroup()

## syncing with the later years
uof_13 <- uof_13 |> 
  select(-max_force_officer, -incident_type, -received_date, -force_officer) |> 
  rename("id" = database_row_number) |> 
  mutate(badge_number = as.character(badge_number),
         id = as.character(id))  
  

## binding together the use of force
uof_cleaned <- uof_mary |>
  bind_rows(uof_13) 



# dropping unnecessary information for merging purposes -------------------

## getting rid of duplicates that were weird and did not make sense.
uof_cleaned <- uof_cleaned %>% 
  drop_na(date_of_occurrence, badge_number) %>% 
  distinct() %>% 
  filter(!(id == "80-15-052109" & force_used_1 == "[no force entered]")) %>%
  filter(!(id == "air14-522" & force_used_1 == "take down")) %>% 
  filter(!(id == "air14-240" & force_used_1 == "take down")) %>% 
  filter(!(id == "80-15-000544"& officer_involved == "incident_officer_1")) %>% 
  distinct(badge_number, date_of_occurrence, time_of_occurrence,
          officer_name, citizen_race, citizen_gender, .keep_all = T) 

uof_cleaned %>% 
  write_csv("created_data/louisville/uof_cleaned.csv")
