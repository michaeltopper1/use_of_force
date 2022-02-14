## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-21
##

library(tidyverse)
library(pdftools)


files_18 <- list.files("raw_data/uof_png_18/", pattern = ".png$")
pdf_2018_text <- map(files_18, ~tesseract::ocr(paste0("raw_data/uof_png_18/", .)))
save(pdf_2018_text, file = "raw_data/use_of_force/uof_18.Rdata")


files_19 <- list.files("raw_data/uof_png_19/", pattern = ".png$")
pdf_2019_text <- map(files_19, ~tesseract::ocr(paste0("raw_data/uof_png_19/", .)))
save(pdf_2019_text, file = "raw_data/use_of_force/uof_19.Rdata")

files_20 <- list.files("raw_data/uof_png_20/", pattern = ".png$")
pdf_2020_text <- map(files_20, ~tesseract::ocr(paste0("raw_data/uof_png_20/", .)))
save(pdf_2020_text, file = "raw_data/use_of_force/uof_20.Rdata")