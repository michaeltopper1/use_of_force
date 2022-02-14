## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-22
##

library(tidyverse)



## removes the blank lines from the file
remove_blank_lines <- function(data) {
  new_data <- data[!data %in% ""]
  return(new_data)
}
load("raw_data/use_of_force/uof_19.Rdata")

name <- "uof_19"
assign(name, pdf_2018_text)

uof_19 <- uof_19 %>% 
  unlist() %>% 
  pdfhelper::split_lines() %>% 
  remove_blank_lines()
  
date_received_indice <- pdfhelper::extract_indexes(uof_19, regex = "^date received", line = 1)
record_id_indice <- pdfhelper::extract_indexes(uof_19, "^record id number", line = 1)
police_officer <- pdfhelper::extract_indexes(uof_19, "^incident officer", line =1)



date_received <- uof_19[date_received_indice] %>% 
  as_tibble() %>% 
  separate(value, c("date_received", "date_occurred", "time_occurrence"), sep = "\\s") %>% 
  mutate(across(starts_with("date"), ~lubridate::mdy(.)))

record_id <- uof_19[record_id_indice] %>% 
  as_tibble() %>% 
  separate(value, c("record_id", "incident_id", 'ia_no'), sep = "\\s") %>% 
  mutate(across(everything(), ~str_trim(.)))

officers <- uof_19[police_officer] %>% 
  as_tibble() %>% 
  separate(value, c("police officer", "badge"), "-") %>% 
  mutate(across(everything(), ~str_trim(.)))


## to get all police officers, start at the incident officers index and go between the indexes
## from here, match on police officer or police sergeant
## paste each of these into either 2 rows or the same row separated by  delimiter
