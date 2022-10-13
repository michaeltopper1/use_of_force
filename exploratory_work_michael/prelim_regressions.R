library(tidyverse)
library(fixest)
library(modelsummary)

shifts_uof <- read_csv("created_data/louisville/shifts_uof_merged_daily.csv")

shifts_uof |> 
  colnames()
shifts_uof <- shifts_uof |> 
  mutate(shift_uof = ifelse(!is.na(uof_id), 1, 0)) 

shifts_uof <- shifts_uof |> 
  group_by(shift_year, shift_month) |> 
  mutate(year_by_month = cur_group_id()) |> 
  ungroup() |> 
  group_by(shift_year, shift_month, shift_day) |> 
  mutate(year_by_month_by_day = cur_group_id()) |> 
  ungroup()

shifts_uof <- shifts_uof |> 
  mutate(eight_hour = ifelse(shift_length == 8, 1 ,0),
         ten_hour = ifelse(shift_length == 10, 1, 0),
         twelve_hour = ifelse(shift_length == 12, 1, 0))

## should we filter to only 8/10/12 hour shifts?
shifts_uof <- shifts_uof %>% 
  mutate(day_of_week = lubridate::wday(startdate_asdate, label = T),
         uof_hour =lubridate:: hour(time_of_occurrence))
 

reg_1 <- shifts_uof %>% 
  filter(eight_hour == 1 | ten_hour == 1 | twelve_hour == 1) %>%
  feols(shift_uof ~ ten_hour + twelve_hour |
          badge + year_by_month + start_hour + day_of_week +uof_hour, data = .)
## 8/10/12 percent of uof
shifts_uof |> colnames()
  group_by(shift_ett, shift_uof) |> 
  count() |> 
  filter(shift_uof == 1) |> 
  janitor::adorn_totals() |> 
  mutate(fraction = n[[5]]) |> 
  rowwise() |> 
  mutate(fraction = n/fraction)

shifts_uof <- shifts_uof %>% 
  mutate(year_minus = case_when(
    shift_year == 2020 ~0,
    shift_year == 2019 ~1,
    shift_year == 2018 ~2,
    shift_year == 2017 ~3, 
    shift_year == 2016 ~4,
    shift_year == 2015 ~5,
    shift_year == 2014 ~6,
    shift_year == 2013 ~7,
  )) %>% 
  rowwise() %>% 
  mutate(age_at_shift = officer_age - year_minus)

## which officers get what shifts?
shifts_uof %>% 
  ggplot(aes(age_at_shift, y = ..scaled.., fill = shift_ett))+
  geom_density(alpha = 0.4) + 
  labs(x = "Age at time of shift", y = "Density",
       fill = "Shift Hours",
       title = "Distribution of Shifts by Age") +
  ggthemes::scale_fill_stata() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~date_bins)
