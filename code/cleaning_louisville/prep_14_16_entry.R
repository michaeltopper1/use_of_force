library(tidyverse)
library(lubridate)
library(tidytext)

uof <- readxl::read_excel("/Users/michaeltopper/Desktop/use_of_force/uof_2014_2016.xlsx") %>% 
  janitor::clean_names()

forces <- "Verbal Directions|Empty Hand Control|Take Down|Empty Hand Strikes|ECW Cartridge Deployed|Hand Control|Spray|Hobble|Kick|Come-along|OC Spray|Impact Weapon|
Other \\(in narrative\\)|Knee Strike\\(s\\)|K-9 Bite|ECW Stun Feature|\\[No force entered\\]|ECW Arc Displayed"
uof %>% 
  select(allegation_s_force_type_s) %>% 
  head(20)
uof %>% 
  mutate(involved_officer_s = str_replace_all(involved_officer_s, "\\r\\n", "")) %>% 
  mutate(involved_officer_s = str_replace_all(involved_officer_s, "\\]", "\\]; ")) %>% 
  mutate(involved_officer_s = str_trim(involved_officer_s)) %>% 
  mutate(involved_officer_s = str_replace(involved_officer_s, ";$", "")) %>% 
  mutate(allegation_s_force_type_s= str_replace_all(allegation_s_force_type_s, "\\r\\n", "")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Verbal Directions", "Verbal Directions; ")) %>% 
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Take Down", "Take Down; ")) %>% 
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Empty Hand Control", "Empty Hand Control; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Empty Hand Strikes", "Empty Hand Strikes; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "ECW Cartridge Deployed", "ECW Cartridge Deployed; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Hand Control", "Hand Control; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Spray", "Spray; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Hobble", "Hobble; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Kick", "Kick; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Come-along", "Come-along; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "OC Spray", "OC Spray; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Impact Weapon", "Impact Weapon; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Other \\(in narrative\\)", "Other (in narrative); ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "Knee Strike\\(s\\)", "Knee Strike(s); ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "K-9 Bite", "K-9 Bite; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "ECW Stun Feature", "ECW Stun Feature; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "\\[No force entered\\]", "[No force entered]; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s,
                                                     "ECW Arc Displayed", "ECW Arc Displayede; ")) %>%
  mutate(allegation_s_force_type_s = str_replace_all(allegation_s_force_type_s, ";\\s;", ";")) %>%
  mutate(allegation_s_force_type_s = str_trim(allegation_s_force_type_s)) %>% 
  mutate(allegation_s_force_type_s = str_replace(allegation_s_force_type_s, ";$", "")) %>% 
  
  select(-x2,-x3,-x8) %>% 
  mutate(across(starts_with("date"), ~lubridate::as_date(.))) %>% 
  write_csv("raw_data/louisville/use_of_force/uof_14_16.csv")
