library(tdcR)
library(tidyverse)


get_site_data <- function(endpoint = endpoint) {
  get_sites(endpoint = endpoint) %>%
    mutate(
      longitude_ = longitude,
      latitude_ = latitude
    ) %>%
    st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
    st_transform(crs = 2193) %>% 
    mutate(
      easting = st_coordinates(.)[, "X"],
      northing = st_coordinates(.)[, "Y"]
    )
} 


get_flow_data <- function(from = "", to = "") {
  get_data_collection(
    collection = "ActiveFlowSites", from = from, to = to
  ) %>%
    rename(flow = value) %>%
    group_by(site) %>%
    slice(-1) %>%
    ungroup()
}


get_rainfall_data <- function(endpoint = endpoint, from = from, to = "") {
  get_data_collection(collection = "Rainfall", method = "Total", time_interval = NA, from = from, to = to, interval = "1 hour", alignment = "00:00") %>%
    rename(rainfall_total = value) %>%
    group_by(site) %>%
    arrange(site, datetime) %>%
    # slice(-1) %>% # remove first row due to offset, datetime refers to start of interval.
    ungroup() %>%
    mutate(
      rainfall_total = round(rainfall_total, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total))
}


get_rainfall_monthly_data <- function(endpoint = endpoint, collection = "Rainfall", from = "", to = "", month = "Aug") {
  get_data_collection(endpoint = endpoint, collection = collection, from = from, to = to) %>% 
    group_by(site, month) %>% 
    summarise(
      historical_month_rainfall_total = mean(rainfall_total, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    filter(month == !!month)
}