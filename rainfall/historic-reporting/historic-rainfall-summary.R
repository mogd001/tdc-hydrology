library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)

source("functions.R")

# Compute summary statistics for each hydro-rainfall site

from <- "Data start"
to <- "Data end"

rainfall <- get_rainfall_monthly_data(from = from, to = to)

# current year 2023
current_year <- 2023

# absolute maximums
site_maximums <- rainfall %>% 
  group_by(site) %>% 
  slice(which.max(rainfall_total_mm))

filter_list <- c(NA, 10, 5, 2) # all years, 10 years, 5 years, 2 years
names_summary <- c("all_years", "past_10_years", "past_5_years", "past_2_years")

rainfall_summary <- lapply(filter_list, summarise_rainfall, rainfall)
names(rainfall_summary) <- names_summary

rainfall_summary_by_site <- list("site_maximums" = site_maximums) %>% 
  append(rainfall_summary)

saveRDS(rainfall_summary_by_site, glue("outputs/pre_{current_year}_rainfall_summary_by_site.RDS"))