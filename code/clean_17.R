## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-20
##

library(tidyverse)
library(pdftools)

uof_17 <- pdf_text("raw_data/use_of_force/uof_2017.pdf") %>% 
  pdfhelper::split_lines()

date_received_indice <- pdfhelper::extract_indexes(uof_17, regex = "^date received", line = 2)
record_id_indice <- pdfhelper::extract_indexes(uof_17, "^record id number", line = 2)
police_officer <- pdfhelper::extract_indexes(uof_17, "^police officer|police sergeant.{1,}- \\d\\d\\d\\d")

date_received <- uof_17[date_received_indice] %>% 
  as_tibble() %>% 
  separate(value, c("date_received", "date_occurred", "time_occurrence"), sep = "\\s{4,}") %>% 
  mutate(across(starts_with("date"), ~lubridate::mdy(.)))

record_id <- uof_17[record_id_indice] %>% 
  as_tibble() %>% 
  separate(value, c("record_id", "incident_id", 'ia_no'), sep = "\\s{4,}") %>% 
  mutate(across(everything(), ~str_trim(.)))

uof_17 <- bind_cols(date_received, record_id)

# 
# write_csv(uof_17, file = "created_data/uof_17.csv")

uof_17 %>% 
  head(500)
uof_t <- pdf_convert("raw_data/uof_png_17/uof_2017.pdf", dpi = 600)
?file.rename()
files <- list.files(pattern = ".png")

map(files, ~file.copy(from = ., to = paste0("raw_data/uof_png_17/", .)))
map(files, ~file.remove(.))
