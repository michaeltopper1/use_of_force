# Explore effect of working consecutive shifts (overwork)
#
#   Toshio Ferrazares

library(tidyverse)
library(fixest)
library(lubridate)
library(tidylog)
library(janitor)

# setwd("D:/Research/shifts/use_of_force")
setwd("/Volumes/GoogleDrive-109693337169056844052/My Drive/Research/shifts/use_of_force") # nolint
load("./created_data/chicago/names_of_trr.Rda")
load("./created_data/chicago/trr_data.Rda")

load("./created_data/chicago/names_of_shifts.Rda")
load("./created_data/chicago/on_duty_shifts.Rda")

# prepare shift names with alternative last names
# this is created by hand in clean_shifts.r
has_alt_last_name <- names_of_shifts %>%
    filter(!is.na(last_name2)) %>%
    mutate(last_name = last_name2)

# first merge name_files to use as bridge between
# trr data and daily shift data
# then check for matches on manual name changes and clean
names_matched <- left_join(names_of_trr, names_of_shifts,
    by = c(
        "year_of_birth", "sex", "appointed_date",
        "first_name", "middle_initial", "last_name"
    )
) %>%
    left_join(has_alt_last_name,
        by = c(
            "year_of_birth", "sex", "appointed_date",
            "first_name", "middle_initial", "last_name"
        )
    ) %>%
    mutate(id = ifelse(is.na(id.x), id.y, id.x)) %>%
    select(
        id, trr_id, year_of_birth, sex, appointed_date,
        first_name, middle_initial, last_name
    )

# check here, seems dates are off for watch 1
on_duty_shifts <- on_duty_shifts %>%
    mutate(date = if_else(watch == 1, date - days(1), date)

# only using 01/2014-12/2019
trr_data <- trr_data %>%
    filter(date >= "2014-01-01" & date <= "2019-12-31")
on_duty_shifts <- on_duty_shifts %>%
    filter(date >= "2014-01-01" & date <= "2019-12-31")

# merge id from shifts into trr data
trr_data <- trr_data %>% left_join(names_matched)

# using officer demographics from shift data
trr_data <- trr_data %>% subset(
    select = -c(
        unit, star, year_of_birth, appointed_date,
        sex, first_name, last_name, middle_initial
    )
)

# merge trr data to shift data using ids
joined_shifts <- left_join(on_duty_shifts, trr_data, by = c("id", "date"))

# check if reports are within shift time
joined_shifts <- joined_shifts %>% mutate(
    within_shift = if_else(
        incident_date_time < shift_end + hours(6) &
            incident_date_time > shift_start - hours(6),
        1, 0
    )
)

# save trr reports that failed to match to a shift
unjoined_reports <- anti_join(trr_data, on_duty_shifts, by = c("id", "date"))

# save trr reported that succesfully matched to shifts
success_reports <- joined_shifts %>%
    filter(within_shift == 1) %>%
    select(colnames(trr_data))

# add in force reports that happen outside to shift times of that day
# to the reports that did not merge to a day (unjoined_reports)
unjoined_reports <- joined_shifts %>%
    # rename(
    #     last_name = last_name.y,
    #     first_name = first_name.y,
    #     middle_initial = middle_initial.y,
    #     star = star.y,
    #     sex = sex.y,
    #     appointed_date = appointed_date.y,
    #     year_of_birth = year_of_birth.y,
    #     unit = unit.y
    # ) %>%
    filter(within_shift == 0) %>%
    select(colnames(trr_data)) %>%
    bind_rows(unjoined_reports)


# retrieve hours from trr reports
# temporary fix is to assign trr reports
# that occur late into the night to the
# shift of the previous day since likely
# night shift
unjoined_reports <- unjoined_reports %>%
    separate(time,
        into = c("hour", "minute"),
        sep = ":", remove = FALSE, convert = TRUE
    )

# change date of unjoined reports based
# on closest day (note poor naming, these are not shifts)
unjoined_dates_adjusted <- unjoined_reports %>%
    mutate(date = if_else(
        hour >= 12,
        ymd(date) + days(1), ymd(date) - days(1)
    ))

# merge in adjusted date reports
joined_shifts <- left_join(
    on_duty_shifts, unjoined_dates_adjusted,
    by = c("id", "date")
)

# check if within shift range
joined_shifts <- joined_shifts %>% mutate(
    within_shift2 = if_else(
        incident_date_time < shift_end + hours(6) &
            incident_date_time > shift_start - hours(6),
        2, 0
    )
)

# take succesful matches and join them to success_reports
success_reports <- joined_shifts %>%
    filter(within_shift2 == 2) %>%
    select(colnames(trr_data)) %>%
    bind_rows(success_reports)


# unjoined reports from the adjusted set
# also correct dates back to original
unjoined_reports <- anti_join(unjoined_dates_adjusted, on_duty_shifts)

# add in unjoined reports to reports outside of shift range
unjoined_reports <- joined_shifts %>%
    filter(within_shift2 == 0) %>%
    select(colnames(trr_data)) %>%
    bind_rows(unjoined_reports) %>%
    separate(time,
        into = c("hour", "minute"),
        sep = ":", remove = FALSE, convert = TRUE
    ) %>%
    mutate(date = if_else(
        hour >= 12,
        ymd(date) - days(1), ymd(date) + days(1)
    ))



# for manual inspection
unjoined_reports %>% left_join(names_of_trr) %>%
write_csv(file = "./unjoined_reports.csv")





# left off here



# try to round in the opposite way
unjoined_dates_adjusted2 <- unjoined_reports %>%
    mutate(date = if_else(
        hour >= 12,
        ymd(date) - days(2), ymd(date) + days(2)
    )) %>%
    select(
        id, date, hour, subject_actions, member_actions, incident_date_time
    ) %>%
    rename( # rename is so that if there are 2 reports that merge in, keep both
        subject_actions3 = subject_actions,
        member_actions3 = member_actions,
        incident_date_time3 = incident_date_time,
        hour3 = hour
    )


# merge in adjusted date reports
joined_shifts <- left_join(
    joined_shifts, unjoined_dates_adjusted2,
    by = c("id", "date")
)

# check if within shift range
joined_shifts <- joined_shifts %>% mutate(
    within_shift3 = if_else(
        incident_date_time2 < shift_end + hours(6) &
            incident_date_time2 > shift_start - hours(6),
        3, 0
    )
)








# for manual inspection
unjoined_dates_adjusted %>%
    anti_join(joined_shifts) %>%
    write_csv(file = "./unjoined_reports2.csv")




# for manual inspection
on_duty_shifts %>%
    filter(id == 10765) %>%
    write_csv(file = "./officer_export.csv")








# data set that shifts days by 2 for reports that
# still do not match to on duty shifts
unjoined_reports2 <- anti_join(
    unjoined_dates_adjusted, joined_shifts,
    by = c("id", "date")
) %>%
    mutate(date = if_else(hour2 >= 12,
        ymd(date) - days(2),
        ymd(date) + days(2)
    )) %>%
    rename(
        subject_actions3 = subject_actions2,
        member_actions3 = member_actions2
    )

# merge together the "corrected" dates
# in total 26,943 shifts matched on the first
# attempt, rounding by 1 day added 1,153 more shifts
# rounding 2 days added 469 more shifts. 5,266 never match
joined_shifts2 <- left_join(
    joined_shifts, unjoined_shifts,
    by = c("id", "date")
) %>%
    left_join(unjoined_shifts2, by = c("id", "date")) %>%
    mutate(has_trr = if_else(
        !is.na(subject_actions) | !is.na(subject_actions2) | !is.na(subject_actions3), # nolint
        1,
        0
    ))

# creating some indicators for regressions
joined <- joined_shifts2 %>% mutate(
    injured_subject = if_else(
        subject_alleged_inj == "Yes" | subject_injured == "Yes", 1, 0
    ),
    first_day_on = if_else(first_day_on == TRUE, 1, 0),
    last_day_on = if_else(last_day_on == TRUE, 1, 0),
    trr_filed = if_else(is.na(trr_id), 0, 1),
    day_worked_number_2 = day_worked_number * day_worked_number
)

# time controls for regressions
joined <- joined %>% mutate(
    month_year = format(date, "%m/%Y"),
    day_of_week = format(date, "%A"),
    year = format(date, "%Y"),
    day_of_year = format(date, "%j"),
    month_of_year = format(date, "%m"),
    month_and_day = format(date, "%m/%d"),
    day_worked_number2 = day_worked_number * day_worked_number
)

# bin together days worked number
# any number <5 is implausible unless overtime
joined <- joined %>% mutate(
    day_worked_number_binned = ifelse(day_worked_number > 5,
        6, day_worked_number
    )
)


# leaving off here on 09/20/22
# there are some regressions below
# but not the most up to date ones

# the data is good to go from here though
# but does rely on the matching algorithm
# above and does not use the 5266 reports that
# are not yet matched (i am not TOO worried about these)


























# filter(is.na(lagged_absence_descr) |
# (lagged_absence_descr != "INJURED ON DUTY" &
# lagged_absence_descr != "REDACTED"  &
# lagged_absence_descr != "PERSONAL DAY" &
# lagged_absence_descr != "EXCUSED FROM DUTY NON DISCIPLINARY" &
# lagged_absence_descr != "OTHER")) %>%

# unit FE, injured subject
joined %>%
    filter(title_cd == 9161 | title_cd == 9171 | title_cd == 9173) %>%
    feols(
        fml = injured_subject
        ~ day_worked_number | unit.x + month_of_year + day_of_week,
        data = .,
        cluster = "unit.x "
    ) %>%
    summary()

# officer FE, injured subject
joined %>%
    filter(title_cd == 9161 | title_cd == 9171 | title_cd == 9173) %>%
    feols(
        fml = injured_subject
        ~ day_worked_number | id + month_of_year + day_of_week,
        data = .,
        cluster = "id"
    ) %>%
    summary()

# officer FE, filed TRR
joined %>%
    filter(title_cd == 9161 | title_cd == 9171 | title_cd == 9173) %>%
    feols(
        fml = has_trr
        ~ day_worked_number | id + month_of_year + day_of_week,
        data = .,
        cluster = "id"
    ) %>%
    summary()


# Unit FE, filed TRR
joined %>%
    feols(
        fml = has_trr
        ~ day_worked_number | unit.x + month_of_year + day_of_week,
        data = .,
        cluster = "unit.x "
    ) %>%
    summary()


# officer FE, injured subject
joined %>%
    filter(title_cd == 9161 | title_cd == 9171 | title_cd == 9173) %>%
    feols(
        fml = injured_subject
        ~ day_worked_number | id + month_of_year + day_of_week,
        data = .,
        cluster = "id"
    ) %>%
    summary()

# with day FE
# officer FE, filed TRR
joined %>%
    filter(title_cd == 9161 | title_cd == 9171 | title_cd == 9173) %>%
    feols(
        fml = has_trr
        ~ day_worked_number | id + date,
        data = .,
        cluster = "id"
    ) %>%
    summary()


# with squared term
# officer FE, injured subject
joined %>%
    filter(title_cd == 9161 | title_cd == 9171 | title_cd == 9173) %>%
    feols(
        fml = injured_subject
        ~ day_worked_number + day_worked_number2 | id + date,
        data = .,
        cluster = "id"
    ) %>%
    summary()

# try this one!!!!!!!!
# officer FE, filed TRR
joined %>%
    filter(title_cd == 9161 | title_cd == 9171 | title_cd == 9173) %>%
    feols(
        fml = has_trr
        ~ day_worked_number + day_worked_number2 |
            id + month_of_year + day_of_week,
        data = .,
        cluster = "id"
    ) %>%
    summary()



on_duty_shifts %>%
    filter(last_day_on == 1) %>%
    select("lagged_absence_descr") %>%
    table() %>%
    sort()


joined %>%
    filter(last_day_on == 1 & injured_subject == 1) %>%
    select("lagged_absence_descr") %>%
    table() %>%
    sort()


# on_duty_shifts %>% filter(last_day_on == "TRUE") %>%
#   ggplot2::ggplot( aes(x = day_worked_number)) + geom_histogram(binwidth=1) +
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank())
