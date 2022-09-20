# Explore effect of working consecutive shifts (overwork)
#
#   Toshio Ferrazares

library(dplyr)
library(tidyverse)
library(fixest)
library(lubridate)

# setwd("D:/")
load("./created_data/chicago/trr_data.Rda")
load("./created_data/chicago/names_of_trr.Rda")

load("./created_data/chicago/on_duty_shifts.Rda")
load("./created_data/chicago/names_of_shifts.Rda")

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


\
# only using 01/2014-12/2019
trr_data <- trr_data %>% filter(date >= "2014-01-01" & date <= "2019-12-31")

# merge id from shifts into trr data
trr_data <- trr_data %>% left_join(names_matched)

# merge trr data to shift data using ids
joined_shifts <- left_join(on_duty_shifts, trr_data, by = c("id", "date"))

# save trr reports that failed to match to a shift
unjoined_reports <- anti_join(trr_data, on_duty_shifts, by = c("id", "date"))

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

unjoined_shifts <- unjoined_reports %>%
  mutate(date = if_else(
    hour >= 12, ymd(date) + days(1),
    ymd(date) - days(1)
  )) %>%
  filter(date >= "2014-01-01") %>%
  select(id, date, hour, subject_actions, member_actions) %>%
  rename(
    subject_actions2 = subject_actions,
    member_actions2 = member_actions,
    hour2 = hour
  )

unjoined_shifts2 <- anti_join(
  unjoined_shifts, joined_shifts,
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

joined <- joined_shifts2 %>% mutate(
  injured_subject = if_else(
    subject_alleged_inj == "Yes" | subject_injured == "Yes", 1, 0
  ),
  first_day_on = if_else(first_day_on == TRUE, 1, 0),
  last_day_on = if_else(last_day_on == TRUE, 1, 0),
  trr_filed = if_else(is.na(trr_filed), 0, 1),
  day_worked_number_2 = day_worked_number * day_worked_number
)

joined <- joined %>% mutate(
  month_year = format(date, "%m/%Y"),
  day_of_week = format(date, "%A"),
  year = format(date, "%Y"),
  day_of_year = format(date, "%j"),
  month_of_year = format(date, "%m"),
  month_and_day = format(date, "%m/%d"),
  day_worked_number2 = day_worked_number * day_worked_number
)

joined <- joined %>% mutate(
  day_worked_number_binned = ifelse(day_worked_number > 5, 6, day_worked_number)
)

# filter(is.na(lagged_absence_descr) | (lagged_absence_descr != "INJURED ON DUTY" & lagged_absence_descr != "REDACTED"  & lagged_absence_descr != "PERSONAL DAY" &
# lagged_absence_descr != "EXCUSED FROM DUTY NON DISCIPLINARY" & lagged_absence_descr != "OTHER")) %>%

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
    ~ day_worked_number + day_worked_number2 | id + month_of_year + day_of_week,
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
