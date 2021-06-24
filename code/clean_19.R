## Purpose of script: clean 2019
##
## Author: Michael Topper
##
## Date Last Edited: 2021-06-17
##

library(magrittr)
library(tidyverse)
library(pdftools)


uof_19 <- pdf_text("raw_data/uof_2019.pdf")

uof_19 <- uof_19 %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_to_lower()


print("hello World")

# extracting indices ------------------------------------------------------


date_time_indices <- uof_19 %>% str_detect("^incident details") %>% 
  which %>% add(3)

records_indices<- uof_19 %>% str_detect("^record id number") %>% 
  which %>% add(2)

date_entered_indices <-  uof_19 %>% str_detect("^date/time entered") %>% 
  which %>% add(2)

incident_summary <- uof_19 %>% str_detect("^incident summary") %>% 
  which %>% add(1)

incident_summary_end <- uof_19 %>% str_detect("^incident location") %>% 
  which

incident_location_indice <- uof_19 %>% str_detect("^incident location") %>% 
  which %>% add(2)

incident_location_indice_end <- uof_19 %>% str_detect("^use of force details") %>% 
  which

uof_details_indices <- uof_19 %>% str_detect("^weather condition") %>% 
  which %>% add(-2)

uof_weather_indices <- uof_19 %>% str_detect("^weather condition") %>% 
  which %>% add(2)

uof_citizen_injured_indices <- uof_19 %>% str_detect("^citizen injured") %>% 
  which %>% add(2)

uof_citizen_build_indices <- uof_19 %>% str_detect("^citizen build") %>% 
  which %>% add(2)

uof_office_injured_indices <- uof_19 %>% str_detect("^officer(s) injured") %>% 
  which %>% add(2)

reported_involved_citizen_indice <- uof_19 %>% str_detect("^reporting") %>% 
  which 


uof_19[reported_involved_citizen_indice]
# Creating tibble columns -------------------------------------------------

incident_summary_end %>% tail()
date_time <- uof_19[date_time_indices] %>% 
  as_tibble() %>% 
  separate(value, c("date_received", "date_of_occurrence", "time_of_occurrence"), sep = "\\s{3,}", extra = "merge")

records <- uof_19[records_indices] %>% 
  as_tibble() %>% 
  separate(value, c("record_id_number", "indicident_control_number", "ia_no"),
           sep = "\\s{3,}", extra = "merge")

date_entered <- uof_19[date_entered_indices] %>% 
  as_tibble() %>% 
  separate(value, c("date_time_entered", "entered_by"),
           sep = "\\s{5,}", extra = "merge")


incident_summary_text <- uof_19[incident_summary]

## this loop pastes together all incident summaries.
for (i in 1:length(incident_summary_text)){
  for(j in incident_summary[i]:incident_summary_end[i]){
    incident_summary_text[i] <- paste(incident_summary_text[i], uof_19[j+1])
  }
}

address_location <- uof_19[incident_location_indice]

for (i in 1:length(incident_location_indice)){
  for (j in incident_location_indice[i]:incident_location_indice_end[i]){
    address_location[i] <- paste(address_location[i], uof_19[j + 1])
  }
}

uof_details <- uof_19[uof_details_indices] %>% 
  as_tibble() %>% 
  separate(value, c("reason_for_force", "service_rendered", 'more_than_1_citizen'),
           sep = "\\s{3,}")

uof_weather <- uof_19[uof_weather_indices] %>% 
  as_tibble() %>% 
  separate(value, c("weather_condition", "light condition", "distance_to_citizen"),
           sep = "\\s{3,}",extra = "merge")
