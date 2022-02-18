#Script to import TRR Data
#
#   Toshio Ferrazares

library(dplyr)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
 
TRR_data <-  read_excel("./raw_data/chicago/use_of_force/P703205_Ferrazares_TRR_1Jan2004_31Dec2020.xlsx", sheet = "Data") %>% 
				clean_names() %>%
				rename(last_name = member_last_name, first_name = member_first_name, middle_initial = member_m_i, year_of_birth = member_birth_year, sex = member_sex) %>%
				separate(col =incident_date_time, into = c("date", "time"), "\\s")

# format date and time
TRR_data <- TRR_data %>% mutate(date = dmy(date),
								appointed_date = ymd(appointed_date),
								year_of_birth = as.double(year_of_birth))



# Remove trailing or leading spaces from names (same as clean_shifts.R)
TRR_data$last_name <- TRR_data$last_name %>% 
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
TRR_data %>%
  mutate_at(vars(first_name, middle_initial, last_name), str_trim)

names_of_TRR <- TRR_data %>% filter(date >= "2014-01-01" & date <= "2019-12-31") %>%
                select(year_of_birth, sex, appointed_date, first_name, middle_initial, last_name) %>% 
								distinct() %>%
                mutate(TRR_id = row_number(),
                  across(c(first_name, middle_initial, last_name), str_squish))


# individual corrections for TRR names
names_of_TRR <- names_of_TRR %>% 
    mutate(middle_initial = if_else(first_name == "ROBERT J", "J", middle_initial),
          first_name      = if_else(first_name == "ROBERT J", "ROBERT", first_name),
          last_name      = if_else(last_name == "DENNIS K", "DENNIS", last_name)) # add correct num of spaces

    # note christopher cannata is actually christoph, need to correct

names_of_shifts <- read_csv(file="./created_data/names_of_shifts.csv")

load("/Volumes/GoogleDrive/My Drive/Research/Shifts/Processed Data/names_of_shifts.Rda")

has_alt_last_name <- names_of_shifts %>% filter(!is.na(last_name2)) %>% mutate(last_name = last_name2)


names_matched <- left_join(names_of_TRR, names_of_shifts, by = c("year_of_birth", "sex", "appointed_date", "first_name", "middle_initial", "last_name")) %>%
                 left_join(has_alt_last_name, by = c("year_of_birth", "sex", "appointed_date", "first_name", "middle_initial", "last_name")) %>%
                 mutate(id = ifelse(is.na(id.x), id.y, id.x)) %>%
                 select(id, year_of_birth, sex, appointed_date, first_name, middle_initial, last_name, TRR_id)

# if want to check for unmatched names:
# names_unmatched <- names_matched %>% filter(is.na(id))

write_csv(names_matched, file="./created_data/names_of_trr.csv")

















# # scrath work
# load("/Volumes/GoogleDrive/My Drive/Research/Shifts/Processed Data/names_of_shifts.Rda")
# # check consistency between my names files
# names_matched <- inner_join(names_of_TRR, names_of_shifts)

# names_unmatched <- anti_join(names_of_TRR, names_of_shifts)


# names_of_TRR <- names_of_TRR %>% left_join(names_of_shifts)

# # merge back in TRR_id and id from shifts
# TRR_data <- TRR_data %>% left_join(names_of_TRR)

# # indicator for TRR filed on day
# TRR_data <- TRR_data %>% mutate(trr_filed = 1)        







