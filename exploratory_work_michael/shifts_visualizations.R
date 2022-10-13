library(tidyverse)
library(lubridate)
library(modelsummary)
library(kableExtra)

shifts_uof <- read_csv("created_data/louisville/shifts_uof_merged_daily.csv")

officers <- read_csv("created_data/louisville/officer_demographics_cleaned.csv")



shift_distribution <- shifts_uof |> 
  filter(shift_length < 24 & shift_length > 0) |>
  ggplot(aes(shift_length, ..density..)) +
  geom_histogram(alpha = 0.5) +
  geom_density(aes(x = shift_length, y = ..scaled..)) +
  scale_x_continuous(breaks = c(1:20)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Shift Hours", y = "Density") +
  theme_minimal()
  

discont_figure <- shifts_uof |> 
  group_by(date_year_month, shift_ett) |> 
  count() |> 
  ggplot(aes(date_year_month, n, color = shift_ett)) +
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = as_date("2015-09-01"), linetype = "dotted") +
  geom_vline(xintercept = as_date("2016-05-01"), linetype = "dotted") +
  labs(color = "Shift Length", x = "", y = "Frequency",
       title = "Frequency of Shift Lengths Over Sample Period") +
  # scale_color_discrete(breaks = c("8 Hour", "10 Hour", "12 Hour", "Other")) +
  scale_color_manual(values =c("8 Hour" = "#c10534", "10 Hour" = "#e37e00", 
                               "12 Hour" = "#1a476f", "Other" = "grey") ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## which shifts have uof the most?
shifts_uof |> 
  distinct(uof_id,
           date_year_month, shift_ett) |>
  count(date_year_month, shift_ett, sort = T) |> 
  ggplot(aes(date_year_month, n, color = shift_ett)) + 
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = as_date("2015-09-01"), linetype = "dotted") +
  geom_vline(xintercept = as_date("2016-05-01"), linetype = "dotted") +
  scale_color_manual(values =c("8 Hour" = "#c10534", "10 Hour" = "#e37e00", 
                               "12 Hour" = "#1a476f", "Other" = "grey") ) +
  scale_x_date(breaks = c(as_date("2013-01-01"), as_date("2015-09-01"),
                          as_date("2015-01-01"), as_date("2016-05-01"),
                          as_date("2017-01-01"), as_date("2019-01-01")),
               labels = scales::date_format("%b '%y")) +
  labs(x = "", y = "Frequency of UOF Incidents", 
       title = "Frequency of Distinct UOF Incidents Over Time",
       subtitle = "Counts are at the monthly level",
       color = "Shift Length") +
  theme_minimal() +
  theme(legend.position = "bottom")
  

uof_shifts_figure <- shifts_uof |> 
  distinct(uof_id, date_year_month) |> 
  count(date_year_month) |> 
  ggplot(aes(date_year_month, n)) + 
  geom_point() +
  geom_vline(xintercept = as_date("2015-09-01"), linetype = "dotted") +
  geom_vline(xintercept = as_date("2016-05-01"), linetype = "dotted") +
  scale_x_date(breaks = c(as_date("2013-01-01"), as_date("2015-09-01"),
                          as_date("2015-01-01"), as_date("2016-05-01"),
                          as_date("2017-01-01"), as_date("2019-01-01")),
               labels = scales::date_format("%b '%y")) +
  labs(x = "", y = "Frequency", title = "Frequency of Distinct UOF Report Total",
       subtitle = "Counts are at the monthly level")+
  theme_minimal()

## which division is the most prone to UOF?
uof_prevalence_table <- shifts_uof |> 
  mutate(shift_uof = ifelse(!is.na(uof_id), 1, 0)) |> 
  filter(shift_uof == 1) |> 
  group_by(officer_division) |> 
  count() |> 
  ungroup() |> 
  mutate(total = sum(n, na.rm = T)) |> 
  rowwise() |> 
  arrange(desc(n)) |> 
  mutate(proportion = n/ total) |> 
  head(10) |> 
  janitor::adorn_totals() |> 
  mutate(total = 5723) |> 
  kbl(col.names = c("Officer Division", "Number of Force Incidents", "Total Number of Force Incidents", "Fraction of Total"),
      booktabs = T, digits = 3,
      caption = "\\label{uof_prevalence_table}The 10 Division with Most Frequent UOF") |> 
  kable_styling(latex_options = "hold_position") 

division_race_table <- shifts_uof |> 
  mutate(shift_uof = ifelse(!is.na(uof_id), 1, 0)) |> 
  filter(officer_division %in% c("1st division", "2nd division", "3rd division",
                                 "4th division", "5th division", "6th division",
                                 "7th division", "8th division")) |> 
  group_by(officer_division, shift_uof) |> 
  summarize(officer_age = mean(officer_age, na.rm = T),
            officer_white = mean(officer_white)) |> 
  pivot_wider(names_from = shift_uof, values_from = c("officer_age", "officer_white")) |> 
  relocate(officer_division, officer_age_0, officer_white_0, officer_age_1, officer_white_1) |> 
  kbl(col.names = c("Officer Division", "Officer Age", "Officer White", "Officer Age", "Officer White"),
      booktabs = T,
      caption = "\\label{division_race_table}Divisions with High UOF") |> 
  kable_styling(latex_options = "hold_position") |> 
  add_header_above(c(" " = 1, "No UOF in Shift" = 2, "UOF reported in Shift" =2))

## differences in a UOF shift and non UOF shift
## pipe was needed here for datasummary...idk why
shifts_uof %>%
  mutate(shift_uof = ifelse(!is.na(uof_id), "Use of Force Shift", "No UOF")) %>%
  mutate(start_hour = hour(startdate)) %>%
  datasummary(shift_length + start_hour +
                officer_age +
                officer_white +
                officer_years_sworn~ shift_uof *(Mean + SD + Min + Max) , data = .)

## do officers switch shifts?
shifts_uof |> 
  group_by(badge, date_bins) |> 
  count(shift_length)
shifts_uof |> 
  count(officer_years_sworn, sort =T)

## distribution of shifts when uof occurs
shifts_uof |> 
  mutate(shift_uof = ifelse(!is.na(uof_id), 1, 0)) |> 
  filter(shift_uof == 1) |> 
  count(shift_length, sort = T) |> 
  ggplot(aes(shift_length, n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.7) +
  scale_x_continuous(breaks = c(1:12)) +
  theme_minimal()





## which officers get what shifts?
shifts_uof %>% 
  ggplot(aes(age_at_shift,  fill = shift_ett))+
  geom_density(alpha = 0.4) + 
  labs(x = "Age at time of shift", y = "Density",
       fill = "Shift Hours",
       title = "Distribution of Shifts by Age") +
  scale_fill_manual(values =c("8 Hour" = "#c10534", "10 Hour" = "#e37e00", 
                               "12 Hour" = "#1a476f", "Other" = "grey") ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~date_bins)

shifts_uof %>% 
  filter(!is.na(termination_date)) %>% 
  filter(startdate > termination_date +days(1) ) %>%
  relocate(termination_date, force_used_1) %>% View()

bad_badges <- shifts_uof %>% 
  mutate(experience_shift = year(startdate) - year(appoint_date), .before = 1) %>% 
  filter(experience_shift < 0) %>% 
  distinct(badge) %>% 
  pull()



shifts_uof %>% 
  group_by(date_year_month, date_bins) %>% 
  mutate(distinct_badges = n_distinct(badge), .before = 1,
         number_shifts = n()) %>% 
  summarize(mean(number_shifts/distinct_badges)) %>% View()


