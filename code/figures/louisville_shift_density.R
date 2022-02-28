## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-24
##

library(tidyverse)
library(patchwork)
library(modelsummary)
library(kableExtra)
theme_set(theme_minimal())

if (!exists("shifts")) {
  shifts <- read_csv("created_data/louisville/shifts.csv")
}


hours_start_density <- shifts %>% 
  filter(shift_start_year < 2020) %>% 
  filter(rank == "POLICE OFFICER") %>% 
  filter(shift_hours %in% c(8,10,12)) %>% 
  mutate(shift_hours = as_factor(shift_hours)) %>% 
  ggplot(aes(x = start_hour, fill = shift_hours)) +
  geom_density(alpha = 0.5) +
  labs(fill = "Shift Hours", x = "Shift Start Hour",  y = "Density", title = "Panel A: Starting Hour") +
  theme(legend.position = "none")

hours_end_density <- shifts %>% 
  filter(shift_start_year < 2020) %>% 
  filter(rank == "POLICE OFFICER") %>% 
  filter(shift_hours %in% c(8,10,12)) %>% 
  mutate(shift_hours = as_factor(shift_hours)) %>% 
  ggplot(aes(x = end_hour, fill = shift_hours)) +
  geom_density(alpha = 0.5) +
  labs(fill = "Shift Hours", x = "Shift Ending Hour",  y = "Density", title = "Panel B: Ending Hour") +
  theme(legend.position = "bottom")

hours_density_plot <- hours_start_density + hours_end_density + plot_layout(ncol = 1)

ggsave(filename = "code/figures/louisville_shift_density.png", hours_density_plot, device = "png",
       bg = "white")
