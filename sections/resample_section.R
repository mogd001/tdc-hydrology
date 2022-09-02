library(tidyverse)

section <- read_csv("data/20220902_mm_section.csv")

final_distance <- seq(0, max(section$distance) + 2, 2) # Check right bound is beyond dataset

updated_section <- as_tibble(approx(section$distance, section$elevation, final_distance, method = "linear")) %>% 
  rename(distance = x, elevation = y)

write_csv(updated_section, paste0("outputs/20220902_mm_section_resampled.csv"))