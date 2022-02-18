#Script to merge shift data (all_shifts) and force data (trr_reports) 
#   Toshio Ferrazares

library(dplyr)
library(tidyverse)
library(janitor)
library(readxl)
library(fixest)
library(lubridate)