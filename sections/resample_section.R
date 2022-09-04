library(tidyverse)

# Resample a section through linear approximation at a given resampling interval

resampling_interval <- 2 # units are m
section <- read_csv("data/20220902_mm_section.csv")

final_distance <- seq(0, max(section$distance) + resampling_interval, resampling_interval) # Check right bound is beyond dataset

updated_section <- as_tibble(approx(section$distance, section$elevation, final_distance, method = "linear")) %>%
  rename(distance = x, elevation = y)

write_csv(updated_section, paste0("outputs/20220902_mm_section_resampled.csv"))
