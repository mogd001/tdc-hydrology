library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)

get_rainfall_monthly_data <- function(endpoint = endpoint, from = "", to = "") {
  # Get rainfall totals by month from collection.
  
  tdcR::get_data_collection(
    collection = "AllRainfall", method = "Total", interval = "1 months",
    from = from, to = to
  ) %>%
    rename(rainfall_total_mm = value) %>%
    mutate(datetime = datetime - months(1),
            month = month(datetime, label = TRUE),
           year = year(datetime),
           year_month = format(datetime, format = "%Y%m")) %>%
    select(site, year, month, year_month, rainfall_total_mm)
}


summarise_rainfall <- function(n_previous_years = NA, rainfall) {
  # Summarise n_previous_years of rainfall data.  If NA 
  # defaults to all years.
  
  if (!is.na(n_previous_years)) {
    rainfall_filtered <- rainfall %>%
      filter(year >= current_year - n_previous_years) 
  } else {
    rainfall_filtered <- rainfall
  }
  
  rainfall_filtered %>% 
    group_by(site) %>% 
    summarise(
      min_year_month = min(year_month, na.rm = TRUE),
      max_year_month = max(year_month, na.rm = TRUE),
      min_rainfall = min(rainfall_total_mm, na.rm = TRUE),
      max_rainfall = max(rainfall_total_mm, na.rm = TRUE),
      median_rainfall = median(rainfall_total_mm, na.rm = TRUE),
      average_rainfall = mean(rainfall_total_mm, na.rm = TRUE)
    )
}

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