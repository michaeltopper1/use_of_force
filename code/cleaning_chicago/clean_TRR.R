# Script to import TRR Data
#
#   Toshio Ferrazares

library(dplyr)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

# setwd("D:/Data/TRR")
setwd("/Volumes/GoogleDrive-109693337169056844052/My Drive/Data/TRR")

trr_data <- read_xlsx(
  "./P703205_Ferrazares_TRR_1Jan2004_31Dec2020.xlsx",
  sheet = "Data"
) %>%
  clean_names() %>%
  rename(
    last_name = member_last_name,
    first_name = member_first_name,
    middle_initial = member_m_i,
    year_of_birth = member_birth_year,
    sex = member_sex
  ) %>%
  separate(
    col = incident_date_time,
    into = c("date", "time"),
    "\\s",
    remove = FALSE
  )

# format date and time
trr_data <- trr_data %>% mutate(
  date = dmy(date),
  appointed_date = ymd(appointed_date),
  year_of_birth = as.double(year_of_birth),
  incident_date_time = dmy_hm(incident_date_time)
)

# Remove trailing or leading spaces from names (same as clean_shifts.R)
trr_data$last_name <- trr_data$last_name %>%
  str_replace(
    pattern     = "MC ",
    replacement = "MC"
  ) %>%
  str_replace(
    pattern     = "- ",
    replacement = "-"
  ) %>%
  str_replace(
    pattern     = " -",
    replacement = "-"
  ) %>%
  str_replace(
    pattern     = "JR.",
    replacement = "JR"
  )

# clean whitespace (same as clean_shifts.R)
trr_data %>%
  mutate_at(vars(first_name, middle_initial, last_name), str_trim)

names_of_trr <- trr_data %>%
  filter(date >= "2014-01-01" & date <= "2019-12-31") %>%
  select(
    year_of_birth, sex, appointed_date,
    first_name, middle_initial, last_name
  ) %>%
  distinct() %>%
  mutate(trr_id = row_number())

trr_data <- trr_data %>% left_join(names_of_trr)

# individual corrections for TRR names
names_of_trr <- names_of_trr %>%
  mutate(
    middle_initial = if_else(first_name == "ROBERT J", "J", middle_initial),
    first_name = if_else(first_name == "ROBERT J", "ROBERT", first_name),
    last_name = if_else(last_name == "DENNIS    K", "DENNIS", last_name)
  ) # add correct num of spaces


# saving as both csv and Rda, should change in
# future to save space but convienent for now
setwd("/Volumes/GoogleDrive-109693337169056844052/My Drive/Research/shifts/Processed Data") # nolint
write_csv(names_of_trr, file = "./names_of_trr.csv")
write_csv(trr_data, file = "./trr_data.csv")

setwd("/Volumes/GoogleDrive-109693337169056844052/My Drive/Research/shifts/use_of_force/created_data/chicago") # nolint
save(trr_data, file = "./trr_data.Rda")
save(names_of_trr, file = "./names_of_trr.Rda")

# !
# note christopher cannata is actually christoph, need to correct
