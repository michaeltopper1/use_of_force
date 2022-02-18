#Script to import and clean shift times for CPD. 
#FOIA request fulfilled via email (UCSB) 08/26/21
#   Toshio Ferrazares

library(tidyverse)
library(janitor)
library(readxl)
library(fixest)
library(lubridate)
library(tidylog, warn.conflicts = FALSE)

setwd("/Volumes/GoogleDrive/My Drive/Michael and Toshio Folder/Data/BWC/Shifts Worked v2 (with times)/raw FOIA data")

# These ended up having same sheets, but keeping in
sheets2014a <- excel_sheets("./A_A-2014-JAN-JUN.xlsx")
sheets2014b <- excel_sheets("./A_A-2014-JUL-DEC.xlsx")
sheets2015a <- excel_sheets("./A_A-2015-JAN-JUN.xlsx")
sheets2015b <- excel_sheets("./A_A-2015-JUL-DEC.xlsx")
sheets2016a <- excel_sheets("./A_A-2016-JAN-JUN.xlsx")
sheets2016b <- excel_sheets("./A_A-2016-JUL-DEC.xlsx")
sheets2017a <- excel_sheets("./A_A-2017-JAN-JUN.xlsx")
sheets2017b <- excel_sheets("./A_A-2017-JUL-DEC.xlsx")
sheets2018a <- excel_sheets("./A_A-2018-JAN-JUN.xlsx")
sheets2018b <- excel_sheets("./A_A-2018-JUL-DEC.xlsx")
sheets2019a <- excel_sheets("./A_A-2019-JAN-JUN.xlsx")
sheets2019b <- excel_sheets("./A_A-2019-JUL-DEC.xlsx")
sheets2020a <- excel_sheets("./A_A-2020-JAN-JUN.xlsx")
sheets2020b <- excel_sheets("./A_A-2020-JUL-DEC.xlsx")
# test

all_shifts <- rbind(
  sheets2014a %>%
    lapply(function(x)
      read_excel("./A_A-2014-JAN-JUN.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2014b %>%
    lapply(function(x)
      read_excel("./A_A-2014-JUL-DEC.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2015a %>%
    lapply(function(x)
      read_excel("./A_A-2015-JAN-JUN.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2015b %>%
    lapply(function(x)
      read_excel("./A_A-2015-JUL-DEC.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2016a %>%
    lapply(function(x)
      read_excel("./A_A-2016-JAN-JUN.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2016b %>%
    lapply(function(x)
      read_excel("./A_A-2016-JUL-DEC.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2017a %>%
    lapply(function(x)
      read_excel("./A_A-2017-JAN-JUN.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2017b %>%
    lapply(function(x)
      read_excel("./A_A-2017-JUL-DEC.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2018a %>%
    lapply(function(x)
      read_excel("./A_A-2018-JAN-JUN.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2018b %>%
    lapply(function(x)
      read_excel("./A_A-2018-JUL-DEC.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2019a %>%
    lapply(function(x)
      read_excel("./A_A-2019-JAN-JUN.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2019b %>%
    lapply(function(x)
      read_excel("./A_A-2019-JUL-DEC.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2020a %>%
    lapply(function(x)
      read_excel("./A_A-2020-JAN-JUN.xlsx", sheet = x)) %>%
    bind_rows(),
  sheets2020b %>%
    lapply(function(x)
      read_excel("./A_A-2020-JUL-DEC.xlsx", sheet = x)) %>%
    bind_rows()
) %>% clean_names()

#change units to double
all_shifts <- all_shifts %>% mutate(unit = as.double(unit))

#create shift length variable (assuming if end time is before start time then shift went to next day)
all_shifts <- all_shifts %>%
  mutate(
    shift_length = ifelse(
      as.numeric(end_time) >= as.double(start_time),
      as.numeric(end_time) - as.double(start_time),
      as.numeric(end_time) + 2400 - as.double(start_time)
    )
  )

# correct appointed date format and date name
all_shifts <- all_shifts %>% mutate(appointed_date = ymd(appointed_date),
                                    year_of_birth = as.double(year_of_birth),
                                    date = round_date(aa_date, unit = "day"))                      

# create dataframe of just the names
names_of_shifts <- all_shifts %>%
  select(name, year_of_birth, appointed_date, year_of_birth, sex) %>%
  as_tibble() %>% distinct()

# remove excess whitespace
names_of_shifts$name <- names_of_shifts$name %>% str_squish()


# separate first, last, and middle initial from name strings, some incorrect ones (e.g. Mc Nulty)
names_of_shifts <-
  names_of_shifts %>%  tidyr::extract(
    name,
    into = c("first_name", "middle_initial" ,"last_name"),
    "(^[A-Z]{1,})\\s([A-Z]\\s)?(.{1,})",
    remove = F
  )


# Remove trailing or leading spaces
names_of_shifts$last_name <- names_of_shifts$last_name %>% 
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


names_of_shifts <- names_of_shifts %>%
  mutate_at(vars(first_name, middle_initial, last_name), str_trim)

# replace blank middle names with missing
names_of_shifts <- names_of_shifts %>% mutate_if(is.character, list(~na_if(.,""))) 

# create IDs for officers, these need to be set once and not changed
names_of_shifts <- names_of_shifts %>% mutate(id = row_number())

# fixing individual names
names_of_shifts <- names_of_shifts %>% 
mutate(first_name = case_when(
  name == "IAN LESTER T BAYOT"    ~ "IAN LESTER",
  name == "AJAMU-GOM C JOHNSON"   ~ "AJAMU-GOM",
  name == "KHIN-FONG KUNG"        ~ "KHIN-FONG",
  name == "MARY KATE GALE"        ~ "MARY KATE",
  name == "JON-MICHAEL R PRONEK"  ~ "JON-MICHAEL",
  name == "RAUL JR CERDA"         ~ "RAUL JR",
  name == "MARY ELLE K MEURIS"    ~ "MARY ELLE",
  name == "JOHN O BRIEN"          ~ "JOHN",
  name == "ZBIG IEW A SNIEZEK"    ~ "ZBIG IEW",
  name == "ROSE ANN PHILLIPS"     ~ "ROSE ANN",
  name == "ABDUL-AZIZ N VHORA"    ~ "ABDUL-AZIZ",
  name == "MICHAEL R G SPIZZIRRI" ~ "MICHAEL R",
  name == "JAN ASHLEY M DEJESUS"  ~ "JAN ASHLEY",
  name == "HUMBERTO F. F CRUZ"  ~ "HUMBERTO F.",
  name == "J'MAL L RILEY"  ~ "J'MAL",
  name == "NOUR EDDINE EL HAMLY"  ~ "NOUR EDDINE",
  name == "REINER JEROME R EBARLE"  ~ "REINER JEROME",
  name == "BRIE ANN R DROZD"  ~ "BRIE ANN",
  name == "CHRISTOPHER J CANNATA"  ~ "CHRISTOPH",
  TRUE ~ first_name))

names_of_shifts <- names_of_shifts %>% 
mutate(middle_initial = case_when(
  name == "IAN LESTER T BAYOT" ~ "T",
  name == "AJAMU-GOM C JOHNSON" ~ "C",
  name == "JON-MICHAEL R PRONEK" ~ "R",
  name == "MICHAEL R G SPIZZIRRI" ~ "G",
  name == "JOHN P O LEARY" ~ "P",
  name == "MICHAEL O SHEA" ~ "NA",
  name == "MARY ELLE K MEURIS" ~ "K",
  name == "JOHN O BRIEN" ~ "NA",
  name == "ZBIG IEW A SNIEZEK" ~ "A",
  name == "ABDUL-AZIZ N VHORA" ~ "N",
  name == "JAN ASHLEY M DEJESUS" ~ "M",
  name == "JOHNNY O DONNELL" ~ "NA",
  name == "HUMBERTO F. F CRUZ"  ~ "F",
  name == "J'MAL L RILEY"  ~ "L",
  name == "REINER JEROME R EBARLE"  ~ "R",
  name == "BRIE ANN R DROZD"  ~ "R",
  TRUE ~ middle_initial))

names_of_shifts <- names_of_shifts %>% 
mutate(last_name = case_when(
  name == "MICHAEL R G SPIZZIRRI" ~ "SPIZZIRRI",
  name == "AJAMU-GOM C JOHNSON"   ~ "JOHNSON",
  name == "MARY KATE GALE"        ~ "GALE",
  name == "RAUL JR CERDA"         ~ "CERDA",
  name == "JOHN P O LEARY"        ~ "O LEARY",
  name == "KHIN-FONG KUNG"        ~ "KUNG",
  name == "MICHAEL O SHEA"        ~ "O SHEA",
  name == "MARY ELLE K MEURIS"    ~ "MEURIS",
  name == "JOHN O BRIEN"          ~ "O BRIEN",
  name == "ZBIG IEW A SNIEZEK"    ~ "SNIEZEK",
  name == "ROSE ANN PHILLIPS"     ~ "PHILLIPS",
  name == "ABDUL-AZIZ N VHORA"    ~ "VHORA",
  name == "JUAN A A ORTIZ"        ~ "ORTIZ",
  name == "MICHAEL R G SPIZZIRRI" ~ "SPIZZIRRI",
  name == "JON-MICHAEL R PRONEK"  ~ "PRONEK",
  name == "IAN LESTER T BAYOT"    ~ "BAYOT",
  name == "JAN ASHLEY M DEJESUS"  ~ "DEJESUS",
  name == "JOHNNY O DONNELL" ~ "O DONNELL",
  name == "HUMBERTO F. F CRUZ"  ~ "CRUZ",
  name == "J'MAL L RILEY"  ~ "RILEY",
  name == "NOUR EDDINE EL HAMLY"  ~ "EL HAMLY",
  name == "REINER JEROME R EBARLE"  ~ "EBARLE",
  name == "BRIE ANN R DROZD"  ~ "DROZD",
  TRUE ~ last_name))

names_of_shifts <- names_of_shifts %>% 
mutate(last_name2 = case_when(
  name == "HARRIET DAVIS" ~ "WHITE",
  name == "SHEENA D JOHNSON" ~ "JOHNSON-PANEK",
  name == "JESSICA L MURPHY" ~ "TRIANTAFILLO",
  name == "SAMANTHA M SZYMANSKI" ~ "LAWLER",
  name == "NANCY D CASTELLANO" ~ "DE LA TORRE",
  name == "SAMANTHA I LACINA" ~ "LACINA-GRANEY",
  name == "MICHELLE T DERTZ" ~ "HELSON",
  name == "ROBERT B RHODES" ~ "STERLING",
  name == "KRISTEN N STURM" ~ "CHICO",
  name == "CAITLIN C TABOR" ~ "RICHARDS",
  name == "TENISHIA C CRAWFORD" ~ "MIRELES",
  name == "DANIELE L BURKS" ~ "DAVIS",
  name == "CARLIE M BANDYK" ~ "CASASANTO",
  name == "CARINA M CRUZ" ~ "MIRANDA",
  name == "KRISTEN M CHORAK" ~ "MARZANO",
  name == "ELIZABETH SANCHEZ" ~ "BAUTISTA",
  name == "KATARZYNA Z SZYMANSKA" ~ "KOPEC",
  name == "ANGELA F OLIFER"  ~ "PEREZ",
  name == "CATHERINE A BODNAR"  ~ "BREEN",
  name == "JENNIFER M FINNEGAN"  ~ "UZUBELL",
  name == "JENNIFER R CUTRONE"  ~ "MAYHEW",
  name == "CAROLINA NAVA"  ~ "OROZCO",
  name == "ASHLEY N MYERS"  ~ "FANTAUZZI",
  name == "GABRIELLE A EZEOLISA"  ~ "TELLES",
  name == "MARIA V MORONES"  ~ "DE LA MORA",
  name == "EDLIN GARCIA"  ~ "RENDON",
  name == "REBECCA P PONTRELLI"  ~ "TIDEI",
  name == "NICOLE L LOZANO"  ~ "OROZCO",
  name == "MARTA LINT"  ~ "KUCHARCZYK",
  name == "ALEXANDRA D HARRIS"  ~ "NITSCHKE",
  name == "ALEXANDRIA D CAMPER"  ~ "WARNER",
  TRUE ~ "NA"))

# merge back in names file, now with ids for each officer
all_shifts <- left_join(all_shifts, names_of_shifts)

# get ready to import units and titles
unit_titles_sheets <- excel_sheets("./P700490_Ferrazares_AAs-2014-2020.xlsx")

# import and merge titles
titles <-
  read_excel("P700490_Ferrazares_AAs-2014-2020.xlsx", sheet = unit_titles_sheets[2]) %>%
  clean_names() %>% rename(title_cd = cd)

all_shifts <- left_join(all_shifts, titles, by = "title_cd")

# import and merge units
units <-
  read_csv("/Volumes/GoogleDrive/My Drive/Michael and Toshio Folder/Data/BWC/Shifts Worked v2 (with times)/units_reshaped.csv") %>% 
  clean_names() %>% 
  rename(unit = cpdunitno, unit1 = descr1, unit2 = descr2, unit3 = descr3) %>%
  mutate(unit = as.double(unit))

all_shifts <- left_join(all_shifts, units, by = "unit")


# lag reason for absence to give stretch stop reason
all_shifts <- all_shifts %>% group_by(id) %>% arrange(date, .by_group = TRUE) %>% 
        mutate(lagged_absence_descr = dplyr::lead(absence_descr)) %>%
        ungroup()


save(all_shifts,file="/Volumes/GoogleDrive/My Drive/Shifts/Processed Data/all_shifts.Rda")
save(names_of_shifts,file="/Volumes/GoogleDrive/My Drive/Shifts/Processed Data/names_of_shifts.Rda")


officer_count <- all_shifts %>%
  filter( title_cd == 9161 | #police officer rank
          title_cd == 9171 | #sergeant of police
          title_cd == 9173   #lieutenant of police
        ) %>% 
  filter(unit > 0 & unit < 26) %>%
  mutate(absence_recode = case_when(absence_code  == "SICKNESS IN FAMILY (SWORN MEMBERS ONLY)"      ~ "sick",
                                 absence_descr    == "SICKNESS INJURED NOT ON DUTY (MEDICAL ROLL)"  ~ "sick",
                                 absence_descr    == "ANNUAL VACATION"                              ~ "vacation",
                                 absence_descr    == "INJURED ON DUTY"                              ~ "injured",
                                 absence_descr    == "DAY OFF"                                      ~ "day_off",
                                 present_for_duty == "Y"                                            ~ "present"
                                 ),
        title_recode = case_when(title_cd    == 9161  ~ "po",
                                 title_cd    == 9171  ~ "serg",
                                 title_cd    == 9173  ~ "lt",
                                 )
         ) %>%
  group_by(date, unit, title_recode, absence_recode) %>%
  summarise(freq = n()) %>%
  ungroup()


officer_count <- pivot_wider(officer_count, names_from = absence_recode, values_from = freq, values_fill = 0) %>%
  pivot_wider(id_cols = c("date", "unit"), names_from = title_recode, values_from = c("day_off", "injured", "present",  "sick", "vacation")) %>%
  select(date, unit, starts_with("present") , starts_with("sick"), starts_with("vacation"), starts_with("injured"), starts_with("day_off)"))%>%
  filter(date >= "2014-01-01" & date <= "2019-12-31")
  
save(officer_count,file="/Volumes/GoogleDrive/My Drive/Shifts/Processed Data/officer_count.Rda")

# 
# count consecutive shifts
# 

# only present officers
on_duty_shifts <- all_shifts %>% filter(present_for_duty == "Y" &
                                        unit > 0 & unit < 26)



# remove officers that have reporting errors (reported working twice in one day)
# some of these could be recovered, but doing so by hand does not need worth it, just dropping all offenders for now
on_duty_shifts <- on_duty_shifts %>% 
                    group_by(id,date) %>% 
                    mutate(reported_shifts = length(id)) %>% 
                    filter(reported_shifts == 1) %>%
                    ungroup()

# create time since last worked variable (diff)
on_duty_shifts <- on_duty_shifts %>% 
                    arrange(date, .by_group = TRUE) %>%
                    group_by(id) %>%
                    mutate(diff = date - dplyr::lag(date)) %>%
                    ungroup()

# create variables based on time since last worked
on_duty_shifts <- on_duty_shifts %>%                     
                    arrange(date, .by_group = TRUE) %>%
                    group_by(id) %>% 
                    mutate(days_off = round(as.double(diff) / 86400) - 1 ,
                           first_day_on = days_off > 0,
                           day_worked_number = ifelse(first_day_on == "TRUE", 1, NA),
                           last_day_on = lead(first_day_on)) %>%
                    ungroup()


on_duty_shifts <- on_duty_shifts %>% group_by(id) %>% arrange(date) %>% 
                      mutate(day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number),
                             day_worked_number = ifelse(is.na(day_worked_number), lag(day_worked_number) + 1, day_worked_number)) %>% 
                      ungroup()


save(on_duty_shifts,file="/Volumes/GoogleDrive/My Drive/Research/Shifts/Processed Data/on_duty_shifts.Rda")

THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 
THIS IS NO LONGER USED PASSED THIS POINT, ANALYSIS MOVED TO analysis_consec_shift.R 

unjoined_reports <- anti_join(TRR_data, on_duty_shifts, by = c("id", "date"))

unjoined_shifts <- unjoined_reports %>% mutate(date = ifelse(time >= 12:00, date + 1, date - 1))

joined <- left_join(on_duty_shifts, TRR_data, by = c("id", "date")) 

joined <- joined %>% mutate(
          injured_subject = ifelse(subject_alleged_inj == "Yes" | subject_injured == "Yes", 1, 0),
          first_day_on = ifelse(first_day_on == TRUE, 1, 0),
          last_day_on = ifelse(last_day_on == TRUE, 1, 0),
          trr_filed = ifelse(is.na(trr_filed), 0, 1),
          day_worked_number_2 = day_worked_number*day_worked_number)

joined <- joined %>% mutate(
          month_year  = format(date, "%m/%Y"),
          day_of_week = format(date, "%A"),
          year = format(date, "%Y"),
          day_of_year = format(date, "%j"),
          month_of_year = format(date, "%m"),
          month_and_day = format(date, "%m/%d"))

joined <- joined %>% mutate(
          day_worked_number_binned = ifelse(day_worked_number > 5, 6, day_worked_number))

# filter(is.na(lagged_absence_descr) | (lagged_absence_descr != "INJURED ON DUTY" & lagged_absence_descr != "REDACTED"  & lagged_absence_descr != "PERSONAL DAY" & 
# lagged_absence_descr != "EXCUSED FROM DUTY NON DISCIPLINARY" & lagged_absence_descr != "OTHER")) %>% 

joined %>% feols( fml = injured_subject
  ~  day_worked_number | unit.x + month_of_year + day_of_week,
  data = .,
  cluster = "unit.x ") %>% summary()





joined %>% feols( fml = trr_filed
  ~  day_worked_number | unit.x + month_of_year + day_of_week,
  data = .,
  cluster = "unit.x ") %>% summary()


# test 





on_duty_shifts %>% filter(last_day_on == 1) %>% select("lagged_absence_descr") %>% table() %>% sort() 


joined %>% filter(last_day_on == 1 & injured_subject == 1) %>% select("lagged_absence_descr") %>% table() %>% sort() 

# tesyt comment 
# on_duty_shifts %>% filter(last_day_on == "TRUE") %>%
#   ggplot2::ggplot( aes(x = day_worked_number)) + geom_histogram(binwidth=1) + 
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank())


