## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-22
##

library(tidyverse)
library(lubridate)


# cleaning shifts data ----------------------------------------------------

sheets <- readxl::excel_sheets("raw_data/louisville/shifts_worked/work_schedules_louisville.xlsx")

shifts <- map_df(sheets, ~readxl::read_excel("raw_data/louisville/shifts_worked/work_schedules_louisville.xlsx", sheet = .) %>% 
      janitor::clean_names() %>% mutate(badge = as.character(badge)))

## looks like i put the floor_date to hours. Shifts start on the hour and end on the hour
## start_date is the floor date of startdate. For instance, 8:30pm will be 8:00pm 
shifts <- shifts %>% 
  mutate(start_date = floor_date(startdate, unit = "hours"),
         end_date = floor_date(enddate, unit = "hours"),
         shift_length = seconds(end_date - start_date)) %>% 
  mutate(shift_length = seconds_to_period(shift_length), .before = 1) %>% 
  mutate(shift_hours = hour(shift_length), .before =1 ) %>% 
  mutate(shift_year = lubridate::year(start_date),
         shift_month = lubridate::month(start_date),
         shift_day = lubridate::day(start_date))  %>% 
  mutate(start_hour = hour(startdate), end_hour = hour(enddate)) %>% 
  filter(shift_year >= 2013)  


## getting rid of duplicates
shifts <- shifts %>% 
  distinct() 


# merging with shifts data ------------------------------------------------

officers <- read_csv("created_data/louisville/officer_demographics_cleaned.csv") %>% 
  mutate(badge = as.character(badge))

## successfully merge with no duplications
shifts_officers <- shifts %>% 
  left_join(officers, by = c("badge"))

## putting in officer experience relative to the end of our sample date.
shifts_officers <- shifts_officers |> 
  mutate(end_sample_year = as_date("2020-01-01"), .before = 1,
         officer_experience = appoint_date %--% end_sample_year/ddays(365.25))

shifts_officers <- shifts_officers %>% 
  mutate(across(where(is.character), ~str_trim(.) %>% str_to_lower())) %>% 
  mutate(switch_date_all_8 = as_date("2015-09-01"),
         switch_date_12 = as_date("2016-05-01")) %>% 
  mutate(date_bins = case_when(
    start_date < switch_date_all_8 ~ "pre",
    start_date >= switch_date_all_8 & start_date < switch_date_12 ~ "mid",
    start_date >= switch_date_12 ~ "post"
  ))



## problem: some shifts have 0 time between them and are actually one shift split into multiple.
## the following gets rid of this issue.


## creating a unique id for each row/shift
shifts_officers <- shifts_officers |> 
  mutate(unique_id = row_number())

## taking out the shifts that are consecutive: e.g., 5-7, 7-8, 8-9
same_starts <- shifts_officers |> 
  group_by(badge) |> 
  arrange(badge, startdate) |> 
  mutate(same_start_end = ifelse(startdate == lag(enddate), 1, 0)) |>
  mutate(same_start_end_before = ifelse(lead(same_start_end) == 1, 1, 0)) |> 
  filter(same_start_end ==1 | same_start_end_before == 1) |> 
  relocate(same_start_end, same_start_end_before)

## grabbing the ids so that I can get rid of these from the shifts_officers
same_starts_ids <- same_starts |> 
  pull(unique_id)

## filtering out the problem shifts
shifts_officers <- shifts_officers |> 
  filter(!unique_id %in% same_starts_ids)

# cleaning the problem shifts ---------------------------------------------


# merging connected start dates -------------------------------------------
##  getting the correct start dates
same_starts_cleaned <- same_starts |> 
  mutate(startdate = case_when(
    startdate == lag(enddate) ~ lag(startdate),
    startdate == lag(enddate, n = 2) ~lag(startdate, n = 2),
    startdate == lag(enddate, n = 3) ~lag(startdate, n = 3),
    TRUE ~ startdate
  )) |>
  mutate(startdate_2 = if_else(startdate == lag(enddate, n = 2), lag(startdate, n = 2), startdate),
         .before=1) |> 
  mutate(startdate_2 = if_else(is.na(startdate_2), startdate, startdate_2)) 

## getting the correct end dates and then making distinct
same_starts_cleaned <- same_starts_cleaned |> 
  group_by(startdate_2, badge) |> 
  mutate(group_id = cur_group_id(), .before = 1) |> 
  mutate(enddate_2 = max(enddate), .before = 1) |> 
  mutate(startdate = startdate_2,
         enddate = enddate_2) |> 
  ungroup() |> 
  select(-enddate_2, -startdate_2, -group_id) |> 
  distinct(badge, startdate, .keep_all = T) 


##now adding back in the problem shifts
shifts_officers <- shifts_officers |> 
  bind_rows(same_starts_cleaned)  

shifts_officers <- shifts_officers |> 
  filter(startdate < as_date("2020-01-01") & startdate >= as_date("2013-01-01")) 
# removing overlapping shifts ---------------------------------------------

## these are shifts in which there is overlap between two shifts in the same officer
## these are dealt with my taking the shift that is the longest
## or, if same length, the shift that is the latest (assuming they're updating schedule)

## checking overlapping shifts and saving
## this also grabs the shift afterwards to compare
overlaps <- shifts_officers |> 
  group_by(badge) |> 
  arrange(startdate) |> 
  mutate(overlap = ifelse(enddate > lead(startdate), 1, 0)) |> 
  filter(overlap == 1 |  lag(overlap) == 1)

## changes the shift length to a period type so I can compare times
if (!class(overlaps$shift_length) == "Period") {
  stop("make sure the shift_length is a period. Uncomment the lines below")
}
# overlaps <- overlaps |>
#   mutate(shift_length = period(shift_length)) |> 
#   relocate(overlap)


## grabbing the overlap ids and removing from shifts
overlaps_ids <- overlaps |> 
  pull(unique_id)

## getting rid of the overlapping shifts from the original data
shifts_officers <- shifts_officers |> 
  filter(!unique_id %in% overlaps_ids)

## this creates the grouping so that i'm looking at every 2 within each officer
## the ntile creates the counting by 11 22 33 etc. within each officer. it is important
## to do this so that I can group by the shift grouping
## this overlaps will be split into two groups and i will bind them back to the shifts_officers
overlaps <- overlaps |> 
  group_by(badge) |> 
  arrange(badge, startdate) |> 
  mutate(enddate_date = as_date(enddate),
         startdate_date = as_date(startdate)) |> 
  add_count() |> 
  mutate(shift_grouping = ntile(1:n, n/2), .before = 1) 


## i take the shift that is the longer shift of the two since I believe
## that they updated the shift later on
overlaps_1 <- overlaps |> 
  group_by(badge, shift_grouping) |> 
  arrange(badge, startdate) |> 
  mutate(shift_length_minutes = minute(minutes(shift_length))) |> 
  mutate(bigger_shift = if_else(shift_length_minutes == max(shift_length_minutes), 1, 0), .before=1) |> 
  filter(bigger_shift == 1) |> 
  mutate(count = n(), .before = 1) |> 
  filter(count == 1) |> 
  ungroup()

## this portion takes the portion of the shift that is the latest if they are the same
overlaps_2 <- overlaps |> 
  group_by(badge, shift_grouping) |> 
  arrange(badge, startdate) |> 
  mutate(shift_length_minutes = minute(minutes(shift_length))) |> 
  mutate(bigger_shift = if_else(shift_length_minutes == max(shift_length_minutes), 1, 0), .before=1) |> 
  filter(bigger_shift == 1) |> 
  mutate(count = n(), .before = 1) |> 
  filter(count >1) |> 
  mutate(latest_startdate = lag(startdate) == 1, .before = 1) |> 
  mutate(latest_startdate = if_else(latest_startdate == F, 1, 0)) |> 
  ungroup() |> 
  drop_na(latest_startdate)

## checking if this worked correctly
nrow(overlaps_1) + nrow(overlaps_2) == nrow(overlaps)/2

## binding back together
shifts_officers <- shifts_officers |> 
  bind_rows(overlaps_1, overlaps_2) 


## saving as csv
shifts_officers %>% 
  write_csv(file = here::here("created_data/louisville/shifts_officers_cleaned.csv"))
