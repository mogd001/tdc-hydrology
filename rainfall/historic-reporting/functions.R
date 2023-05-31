library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}


get_rainfall_daily_data <- function(endpoint = endpoint, site = NA, from = "", to = "") {
  # Get rainfall totals by day.
  if (is.na(site)) {
    tdcR::get_data_collection(
      collection = "AllRainfall", method = "Total", interval = "1 day",
      from = from, to = to
    ) %>%
      rename(rainfall_total_mm = value) %>%
      mutate(
        datetime = with_tz(datetime, tz = "NZ"),
        day = day(datetime),
        date = as.Date(datetime, tz = "NZ"),
        month = month(datetime, label = TRUE),
        year = year(datetime),
        month_day = as.numeric(format(datetime, format = "%m%d"))
      ) %>%
      select(site, year, month, day, date, month_day, rainfall_total_mm)
  } else {
    tdcR::get_data_site_measurement(
      site = site, measurement = "Rainfall", method = "Total", interval = "1 day",
      from = from, to = to
    ) %>%
      rename(rainfall_total_mm = value) %>%
      mutate(
        datetime = with_tz(datetime, tz = "NZ"),
        day = day(datetime),
        date = as.Date(datetime, tz = "NZ"),
        month = month(datetime, label = TRUE),
        year = year(datetime),
        month_day = as.numeric(format(datetime, format = "%m%d")),
        site = site
      ) %>%
      select(site, year, month, day, date, month_day, rainfall_total_mm)
  }
}


get_rainfall_monthly_data <- function(endpoint = endpoint, site = NA, from = "", to = "") {
  # Get rainfall totals by month.
  if (is.na(site)) {
    tdcR::get_data_collection(
      collection = "AllRainfall", method = "Total", interval = "1 months",
      from = from, to = to
    ) %>%
      rename(rainfall_total_mm = value) %>%
      mutate(
        datetime = with_tz(datetime, tz = "NZ"),
        datetime = datetime - months(1),
        month = month(datetime, label = TRUE),
        year = year(datetime),
        year_month = format(datetime, format = "%Y%m")
      ) %>%
      select(site, year, month, year_month, rainfall_total_mm)
  } else {
    r <- tdcR::get_data_site_measurement(
      site = site, measurement = "Rainfall", method = "Total", interval = "1 months",
      from = from, to = to
    ) %>%
      rename(rainfall_total_mm = value) %>%
      mutate(
        datetime = with_tz(datetime, tz = "NZ"),
        datetime = datetime - months(1),
        month = month(datetime, label = TRUE),
        year = year(datetime),
        year_month = format(datetime, format = "%Y%m"),
        site = site
      ) %>%
      select(site, year, month, year_month, rainfall_total_mm)
  }
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
