# Explore effect of 8/12 hour shifts using gonzalo regression
#
#

library(dplyr)
library(tidyverse)
library(fixest)
library(lubridate)
library(tidylog)

setwd("/Volumes/GoogleDrive-109693337169056844052/My Drive/Research/shifts/use_of_force") # nolint

louisville_shifts <- read_csv("./created_data/louisville/shifts_uof_merged_daily.csv") # nolint

louisville_shifts <- louisville_shifts %>% mutate(
    force_used = ifelse(is.na(force_used_1), 0, 1),
    physical_force_used = case_when(
        force_used_1 == "12 ga. sock round" ~ 1,
        force_used_1 == "come-along" ~ 0,
        force_used_1 == "de-escalation techniques" ~ 0,
        force_used_1 == "deadly force used" ~ 1,
        force_used_1 == "ecw arc displayed" ~ 1,
        force_used_1 == "ecw cartridge deployed" ~ 1,
        force_used_1 == "ecw stun feature" ~ 1,
        force_used_1 == "empty hand control" ~ 1,
        force_used_1 == "empty hand strikes" ~ 1,
        force_used_1 == "hobble" ~ 0,
        force_used_1 == "hooble" ~ 0,
        force_used_1 == "impact weapon" ~ 1,
        force_used_1 == "k-9 bite" ~ 1,
        force_used_1 == "kick" ~ 1,
        force_used_1 == "knee strike(s)" ~ 1,
        force_used_1 == "none" ~ 0,
        force_used_1 == "oc spray" ~ 1,
        force_used_1 == "other (in narrative)" ~ 0,
        force_used_1 == "pepper ball system" ~ 1,
        force_used_1 == "special impact munitions" ~ 1,
        force_used_1 == "take down" ~ 1,
        force_used_1 == "verbal direction" ~ 0,
        force_used_1 == "verbal directions" ~ 0,
        TRUE ~ 0
    ),
    physical_force_used = case_when(
        force_used_2 == "12 ga. sock round" ~ 1,
        force_used_2 == "come-along" ~ physical_force_used,
        force_used_2 == "de-escalation techniques" ~ physical_force_used,
        force_used_2 == "deadly force used" ~ 1,
        force_used_2 == "ecw arc displayed" ~ 1,
        force_used_2 == "ecw cartridge deployed" ~ 1,
        force_used_2 == "ecw stun feature" ~ 1,
        force_used_2 == "empty hand control" ~ 1,
        force_used_2 == "empty hand strikes" ~ 1,
        force_used_2 == "hobble" ~ physical_force_used,
        force_used_2 == "hooble" ~ physical_force_used,
        force_used_2 == "impact weapon" ~ 1,
        force_used_2 == "k-9 bite" ~ 1,
        force_used_2 == "kick" ~ 1,
        force_used_2 == "knee strike(s)" ~ 1,
        force_used_2 == "none" ~ physical_force_used,
        force_used_2 == "oc spray" ~ 1,
        force_used_2 == "other (in narrative)" ~ physical_force_used,
        force_used_2 == "pepper ball system" ~ 1,
        force_used_2 == "special impact munitions" ~ 1,
        force_used_2 == "take down" ~ 1,
        force_used_2 == "verbal direction" ~ physical_force_used,
        force_used_2 == "verbal directions" ~ physical_force_used,
        TRUE ~ physical_force_used
    ),
    physical_force_used = case_when(
        force_used_3 == "12 ga. sock round" ~ 1,
        force_used_3 == "come-along" ~ physical_force_used,
        force_used_3 == "de-escalation techniques" ~ physical_force_used,
        force_used_3 == "deadly force used" ~ 1,
        force_used_3 == "ecw arc displayed" ~ 1,
        force_used_3 == "ecw cartridge deployed" ~ 1,
        force_used_3 == "ecw stun feature" ~ 1,
        force_used_3 == "empty hand control" ~ 1,
        force_used_3 == "empty hand strikes" ~ 1,
        force_used_3 == "hobble" ~ physical_force_used,
        force_used_3 == "hooble" ~ physical_force_used,
        force_used_3 == "impact weapon" ~ 1,
        force_used_3 == "k-9 bite" ~ 1,
        force_used_3 == "kick" ~ 1,
        force_used_3 == "knee strike(s)" ~ 1,
        force_used_3 == "none" ~ physical_force_used,
        force_used_3 == "oc spray" ~ 1,
        force_used_3 == "other (in narrative)" ~ physical_force_used,
        force_used_3 == "pepper ball system" ~ 1,
        force_used_3 == "special impact munitions" ~ 1,
        force_used_3 == "take down" ~ 1,
        force_used_3 == "verbal direction" ~ physical_force_used,
        force_used_3 == "verbal directions" ~ physical_force_used,
        TRUE ~ physical_force_used
    ),
    physical_force_used = case_when(
        force_used_4 == "12 ga. sock round" ~ 1,
        force_used_4 == "come-along" ~ physical_force_used,
        force_used_4 == "de-escalation techniques" ~ physical_force_used,
        force_used_4 == "deadly force used" ~ 1,
        force_used_4 == "ecw arc displayed" ~ 1,
        force_used_4 == "ecw cartridge deployed" ~ 1,
        force_used_4 == "ecw stun feature" ~ 1,
        force_used_4 == "empty hand control" ~ 1,
        force_used_4 == "empty hand strikes" ~ 1,
        force_used_4 == "hobble" ~ physical_force_used,
        force_used_4 == "hooble" ~ physical_force_used,
        force_used_4 == "impact weapon" ~ 1,
        force_used_4 == "k-9 bite" ~ 1,
        force_used_4 == "kick" ~ 1,
        force_used_4 == "knee strike(s)" ~ 1,
        force_used_4 == "none" ~ physical_force_used,
        force_used_4 == "oc spray" ~ 1,
        force_used_4 == "other (in narrative)" ~ physical_force_used,
        force_used_4 == "pepper ball system" ~ 1,
        force_used_4 == "special impact munitions" ~ 1,
        force_used_4 == "take down" ~ 1,
        force_used_4 == "verbal direction" ~ physical_force_used,
        force_used_4 == "verbal directions" ~ physical_force_used,
        TRUE ~ physical_force_used
    ),
    physical_force_used = case_when(
        force_used_5 == "12 ga. sock round" ~ 1,
        force_used_5 == "come-along" ~ physical_force_used,
        force_used_5 == "de-escalation techniques" ~ physical_force_used,
        force_used_5 == "deadly force used" ~ 1,
        force_used_5 == "ecw arc displayed" ~ 1,
        force_used_5 == "ecw cartridge deployed" ~ 1,
        force_used_5 == "ecw stun feature" ~ 1,
        force_used_5 == "empty hand control" ~ 1,
        force_used_5 == "empty hand strikes" ~ 1,
        force_used_5 == "hobble" ~ physical_force_used,
        force_used_5 == "hooble" ~ physical_force_used,
        force_used_5 == "impact weapon" ~ 1,
        force_used_5 == "k-9 bite" ~ 1,
        force_used_5 == "kick" ~ 1,
        force_used_5 == "knee strike(s)" ~ 1,
        force_used_5 == "none" ~ physical_force_used,
        force_used_5 == "oc spray" ~ 1,
        force_used_5 == "other (in narrative)" ~ physical_force_used,
        force_used_5 == "pepper ball system" ~ 1,
        force_used_5 == "special impact munitions" ~ 1,
        force_used_5 == "take down" ~ 1,
        force_used_5 == "verbal direction" ~ physical_force_used,
        force_used_5 == "verbal directions" ~ physical_force_used,
        TRUE ~ physical_force_used
    ),
    physical_force_used = case_when(
        force_used_6 == "12 ga. sock round" ~ 1,
        force_used_6 == "come-along" ~ physical_force_used,
        force_used_6 == "de-escalation techniques" ~ physical_force_used,
        force_used_6 == "deadly force used" ~ 1,
        force_used_6 == "ecw arc displayed" ~ 1,
        force_used_6 == "ecw cartridge deployed" ~ 1,
        force_used_6 == "ecw stun feature" ~ 1,
        force_used_6 == "empty hand control" ~ 1,
        force_used_6 == "empty hand strikes" ~ 1,
        force_used_6 == "hobble" ~ physical_force_used,
        force_used_6 == "hooble" ~ physical_force_used,
        force_used_6 == "impact weapon" ~ 1,
        force_used_6 == "k-9 bite" ~ 1,
        force_used_6 == "kick" ~ 1,
        force_used_6 == "knee strike(s)" ~ 1,
        force_used_6 == "none" ~ physical_force_used,
        force_used_6 == "oc spray" ~ 1,
        force_used_6 == "other (in narrative)" ~ physical_force_used,
        force_used_6 == "pepper ball system" ~ 1,
        force_used_6 == "special impact munitions" ~ 1,
        force_used_6 == "take down" ~ 1,
        force_used_6 == "verbal direction" ~ physical_force_used,
        force_used_6 == "verbal directions" ~ physical_force_used,
        TRUE ~ physical_force_used
    ),
    physical_force_used = case_when(
        force_used_7 == "12 ga. sock round" ~ 1,
        force_used_7 == "come-along" ~ physical_force_used,
        force_used_7 == "de-escalation techniques" ~ physical_force_used,
        force_used_7 == "deadly force used" ~ 1,
        force_used_7 == "ecw arc displayed" ~ 1,
        force_used_7 == "ecw cartridge deployed" ~ 1,
        force_used_7 == "ecw stun feature" ~ 1,
        force_used_7 == "empty hand control" ~ 1,
        force_used_7 == "empty hand strikes" ~ 1,
        force_used_7 == "hobble" ~ physical_force_used,
        force_used_7 == "hooble" ~ physical_force_used,
        force_used_7 == "impact weapon" ~ 1,
        force_used_7 == "k-9 bite" ~ 1,
        force_used_7 == "kick" ~ 1,
        force_used_7 == "knee strike(s)" ~ 1,
        force_used_7 == "none" ~ physical_force_used,
        force_used_7 == "oc spray" ~ 1,
        force_used_7 == "other (in narrative)" ~ physical_force_used,
        force_used_7 == "pepper ball system" ~ 1,
        force_used_7 == "special impact munitions" ~ 1,
        force_used_7 == "take down" ~ 1,
        force_used_7 == "verbal direction" ~ physical_force_used,
        force_used_7 == "verbal directions" ~ physical_force_used,
        TRUE ~ physical_force_used
    ),
    physical_force_used = case_when(
        force_used_8 == "12 ga. sock round" ~ 1,
        force_used_8 == "come-along" ~ physical_force_used,
        force_used_8 == "de-escalation techniques" ~ physical_force_used,
        force_used_8 == "deadly force used" ~ 1,
        force_used_8 == "ecw arc displayed" ~ 1,
        force_used_8 == "ecw cartridge deployed" ~ 1,
        force_used_8 == "ecw stun feature" ~ 1,
        force_used_8 == "empty hand control" ~ 1,
        force_used_8 == "empty hand strikes" ~ 1,
        force_used_8 == "hobble" ~ physical_force_used,
        force_used_8 == "hooble" ~ physical_force_used,
        force_used_8 == "impact weapon" ~ 1,
        force_used_8 == "k-9 bite" ~ 1,
        force_used_8 == "kick" ~ 1,
        force_used_8 == "knee strike(s)" ~ 1,
        force_used_8 == "none" ~ physical_force_used,
        force_used_8 == "oc spray" ~ 1,
        force_used_8 == "other (in narrative)" ~ physical_force_used,
        force_used_8 == "pepper ball system" ~ 1,
        force_used_8 == "special impact munitions" ~ 1,
        force_used_8 == "take down" ~ 1,
        force_used_8 == "verbal direction" ~ physical_force_used,
        force_used_8 == "verbal directions" ~ physical_force_used,
        TRUE ~ physical_force_used
    ),
)


# switch1:2015 - 09 - 01
# switch2:2016 - 05 - 01

# create officer types
officer_panel_pre_switch_1 <-
    louisville_shifts %>%
    filter(startdate < "2015-09-01") %>%
    filter(startdate > "2014-09-01") %>%
    select(badge, shift_length, physical_force_used) %>%
    mutate(shift_10_hour = ifelse(shift_length > 9, 1, 0)) %>%
    group_by(badge) %>%
    summarise(
        percent_10_hour = mean(shift_10_hour),
        have_used_phys_force = max(physical_force_used)
    ) %>%
    mutate(
        type_70 = ifelse(percent_10_hour >= 0.70, 1, 0),
        type_80 = ifelse(percent_10_hour >= 0.80, 1, 0),
        type_90 = ifelse(percent_10_hour >= 0.90, 1, 0),
        type_95 = ifelse(percent_10_hour >= 0.95, 1, 0),
        type_98 = ifelse(percent_10_hour >= 0.98, 1, 0),
        type_100 = ifelse(percent_10_hour >= 1, 1, 0)
    )


louisville_shifts_select_1 <- louisville_shifts %>%
    select(
        badge, date_year_month, startdate, shift_length,
        force_used, rank, assignment, physical_force_used,
        start_hour
    ) %>%
    filter(startdate < "2016-05-01") %>%
    filter(startdate > "2014-09-01") %>%
    left_join(officer_panel_pre_switch_1) %>%
    filter(rank == "police officer") %>%
    mutate(
        date_fe = as.Date(startdate),
        shift_10 = ifelse(shift_length == 10, 1, 0),
        shift_12 = ifelse(shift_length == 12, 1, 0),
        post_type_70 = ifelse(startdate > "2015-09-01", type_70, 0),
        post_type_80 = ifelse(startdate > "2015-09-01", type_80, 0),
        post_type_90 = ifelse(startdate > "2015-09-01", type_90, 0),
        post_type_95 = ifelse(startdate > "2015-09-01", type_95, 0),
        post_type_98 = ifelse(startdate > "2015-09-01", type_98, 0),
        post_type_100 = ifelse(startdate > "2015-09-01", type_70, 0)
    )

# officer_panel_post_switch_2 <-
#     louisville_shifts %>%
#     filter(startdate > "2016-05-01") %>%
#     select(badge, shift_length) %>%
#     mutate(shift_12_hour = ifelse(shift_length > 10, 1, 0)) %>%
#     group_by(badge) %>%
#     summarise(percent_12_hour = mean(shift_12_hour)) %>%
#     mutate(
#         type_70 = ifelse(percent_12_hour >= 0.70, 1, 0),
#         type_80 = ifelse(percent_12_hour >= 0.80, 1, 0),
#         type_90 = ifelse(percent_12_hour >= 0.90, 1, 0),
#         type_95 = ifelse(percent_12_hour >= 0.95, 1, 0),
#         type_98 = ifelse(percent_12_hour >= 0.98, 1, 0),
#         type_100 = ifelse(percent_12_hour >= 1, 1, 0)
#     )

# louisville_shifts_select_2 <- louisville_shifts %>%
#     select(
#         badge, date_year_month, startdate, shift_length,
#         force_used, rank, assignment, physical_force_used
#     ) %>%
#     filter(startdate > "2016-05-01") %>%
#     left_join(officer_panel_post_switch_2) %>%
#     filter(rank == "police officer") %>%
#     mutate(
#         date_fe = as.Date(startdate),
#         post_type_70 = ifelse(startdate > "2015-09-01", type_70, 0),
#         post_type_80 = ifelse(startdate > "2015-09-01", type_80, 0),
#         post_type_90 = ifelse(startdate > "2015-09-01", type_90, 0),
#         post_type_95 = ifelse(startdate > "2015-09-01", type_95, 0),
#         post_type_98 = ifelse(startdate > "2015-09-01", type_98, 0),
#         post_type_100 = ifelse(startdate > "2015-09-01", type_70, 0)
#     )

# # time controls for regressions
# louisville_shifts <- louisville_shifts %>% mutate(
#     month_year = format(date, "%m/%Y"),
#     day_of_week = format(date, "%A"),
#     year = format(date, "%Y"),
#     day_of_year = format(date, "%j"),
#     month_of_year = format(date, "%m"),
#     month_and_day = format(date, "%m/%d"),
#     day_worked_number2 = day_worked_number * day_worked_number
# )

louisville_shifts_select_1 %>%
    filter(have_used_phys_force == 1) %>%
    feols(
        fml = physical_force_used
        ~ post_type_70 | badge + date_fe + start_hour,
        data = .,
        cluster = "badge"
    ) %>%
    summary()

louisville_shifts_select_1 %>%
    filter(have_used_phys_force == 1) %>%
    feols(
        fml = physical_force_used
        ~ post_type_80 | badge + date_fe + start_hour,
        data = .,
        cluster = "badge"
    ) %>%
    summary()


louisville_shifts_select_1 %>%
    filter(have_used_phys_force == 1) %>%
    feols(
        fml = physical_force_used
        ~ post_type_90 | badge + date_fe + start_hour,
        data = .,
        cluster = "badge"
    ) %>%
    summary()


louisville_shifts_select_1 %>%
    filter(have_used_phys_force == 1) %>%
    feols(
        fml = physical_force_used
        ~ post_type_95 | badge + date_fe + start_hour,
        data = .,
        cluster = "badge"
    ) %>%
    summary()


louisville_shifts_select_1 %>%
    filter(have_used_phys_force == 1) %>%
    feols(
        fml = physical_force_used
        ~ post_type_98 | badge + date_fe + start_hour,
        data = .,
        cluster = "badge"
    ) %>%
    summary()


louisville_shifts_select_1 %>%
    filter(have_used_phys_force == 1) %>%
    feols(
        fml = physical_force_used
        ~ post_type_100 | badge + date_fe + start_hour,
        data = .,
        cluster = "badge"
    ) %>%
    summary()



louisville_shifts_select_1 %>%
    filter(percent_10_hour > .1) %>%
    feols(
        fml = physical_force_used
        ~ post_type_95 | badge + date_fe + assignment + start_hour,
        data = .,
        cluster = "badge"
    ) %>%
    summary()
