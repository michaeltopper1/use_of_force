library(tidyverse)
library(lubridate)
library(modelsummary)



officer_demographics <- read_csv("created_data/louisville/officer_demographics_cleaned.csv")
unmerged_force <- read_csv("created_data/louisville/unmerged_uof_daily.csv") |> 
  mutate(unmerged = "Not Merged")
merged_force <- read_csv("created_data/louisville/uof_cleaned.csv") |> 
  left_join(officer_demographics, by = c("badge_number" = "badge")) |> 
  filter(!uof_id %in% unmerged_force$uof_id) |> 
  mutate(unmerged = "Merged")

uof <- unmerged_force |> 
  bind_rows(merged_force)

## is there a pattern in time of day?
## appears to be a pattern
unmerged_merged_histogram <- uof |> 
  distinct(uof_id, .keep_all = T) |> 
  mutate(hour =hour(time_of_occurrence), .before =1 ) |> 
  ggplot(aes(hour,y = ..density.., fill = unmerged)) + 
  geom_histogram(aes(hour,y = ..density..)) +
  facet_wrap(~unmerged) +
  geom_density(aes(hour, y = ..density..), alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,24, 2), labels = seq(0,24,2)) +
  labs(x = "Hour of the Day", y = "Count",
       title = "Frequency of Unmerged UOF by Hour",
       fill = " ",
       subtitle = "~9% of UOF unmerged") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggthemes::scale_fill_stata()



## different across different covariates?
# uof_selectcols <- uof |> 
#   select(`Officer Age` = officer_age,`Officer Years Sworn`=  officer_years_sworn,
#          `Officer Female` = officer_female, `Officer Black` = officer_black, 
#          `Officer White` = officer_white,
#          `Officer Asian` = officer_asian,
#          `College Degree` = college, 
#          `Some College` = some_college, 
#          `Graduate Degree` = graduate_degree, 
#          `High School Grad` = high_school_grad,
#           unmerged)
# 
# modelsummary::datasummary_balance(~unmerged,
#                                     data= uof_selectcols)

## different across month and year?

timeplot_uof_unmerged <- uof |> 
  distinct(uof_id, .keep_all = T) |> 
  mutate(year = year(date_of_occurrence),
         month = month(date_of_occurrence),
         year_month = ymd(paste0(year, "-", month, "-1"))) |> 
  group_by(unmerged, year_month) |> 
  mutate(number_force = n()) |> 
  ggplot(aes(year_month, number_force, color = unmerged)) +
  geom_line() +
  ggthemes::scale_color_fivethirtyeight() +
  scale_x_date(breaks = "9 month", date_labels =  "%b %y") +
  labs(y = "Number of Unique Force Incidents",
       x = "",
       title = "Force Incidents Over Time",
       color = "") +
  theme_minimal() +
  ggthemes::scale_color_stata() +
  theme(legend.position = "bottom")


