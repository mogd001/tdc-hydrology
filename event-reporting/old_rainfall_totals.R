library(tidyverse)
library(lubridate)
library(glue)
library(tdcR)
library(sf)
library(plotly)

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


get_rainfall_data <- function(endpoint = endpoint, from = "", to = "") {
  get_data_collection(endpoint = endpoint, collection = "Rainfall", method = "Total", time_interval = NA, from = from, to = to, interval = "1 hour", alignment = "00:00") %>%
    rename(rainfall_total = value) %>%
    group_by(site) %>%
    arrange(site, datetime) %>%
    # slice(-1) %>% # remove first row due to offset, datetime refers to start of interval.
    ungroup() %>%
    mutate(
      interval = hours(1),
      datetime = datetime + interval, # totals with rainfall so add one hour, "rainfall up to hour"
      rainfall_total = round(rainfall_total, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total))
}


get_rainfall_monthly_data <- function(endpoint = endpoint, collection = Rainfall, from = "", to = "") {
  get_data_collection(endpoint = endpoint, collection = collection, method = "Total", time_interval = NA, from = from, to = to, interval = "1 month", alignment = "00:00") %>%
    rename(rainfall_total = value) %>%
    group_by(site) %>%
    arrange(site, datetime) %>%
    # slice(-1) %>% # remove first row due to offset, datetime refers to start of interval.
    ungroup() %>%
    mutate(
      interval = months(1),
      rainfall_total = round(rainfall_total, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total)) %>% 
    group_by(site, month) %>% 
    summarise(
      historical_aug_rainfall_total = mean(rainfall_total, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    filter(month == "Aug")
}


start_date <- ymd("2022-08-16")
end_date <- ymd("2022-08-22")

from <- format(start_date, "%Y%m%d")
to <- format(end_date, "%Y%m%d")

now <- format(Sys.time(), "%Y%m%dT%H%M%S")
now_plot <- str_replace(now, "T", " ")

tdc_endpoint <- "http://envdata.tasman.govt.nz/data.hts?"
mdc_endpoint <- "http://hydro.marlborough.govt.nz/mdc data.hts?"
wgrc_endpoint <- "http://hilltop.wcrc.govt.nz/Websitedata.hts?"

tdc_sites <- get_site_data(endpoint = tdc_endpoint) %>% 
  mutate(region = "nelsontasman")
mdc_sites <- get_site_data(endpoint = mdc_endpoint) %>% 
  mutate(region = "marlborough")
wgrs_sites <- get_site_data(endpoint = wgrc_endpoint) %>% 
  mutate(region = "westcoast")

tdc_rainfall <- get_rainfall_data(endpoint = tdc_endpoint, from = from, to = to)
mdc_rainfall <- get_rainfall_data(endpoint = mdc_endpoint, from = from, to = to)
wgrs_rainfall <- get_rainfall_data(endpoint = wgrc_endpoint, from = from, to = to)

sites <- bind_rows(tdc_sites, mdc_sites, wgrs_sites)
rainfall <- bind_rows(tdc_rainfall, mdc_rainfall, wgrs_rainfall)

# Rainfall event summary
rainfall_sites <- tibble(site = unique(rainfall$site))
rainfall_event_summary <- rainfall %>%
  group_by(site) %>%
  summarise(
    event_rainfall_total = round(sum(rainfall_total, na.rm = TRUE), 0),
    event_max_hrly_rainfall = round(max(rainfall_total, na.rm = TRUE), 0)
  )


# Rainfall August average
tdc_aug_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "Rainfall", from = "Data Start", to = "Data End")
mdc_aug_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = "Data Start", to = "Data End")
wgrs_aug_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = "Data Start", to = "Data End")

aug_rainfall <- bind_rows(tdc_aug_rainfall, mdc_aug_rainfall, wgrs_aug_rainfall) %>% 
  select(-month)

rainfall_out <- rainfall_sites %>%
  left_join(sites, by = "site") %>%
  left_join(rainfall_event_summary, by = "site") %>%
  left_join(aug_rainfall, by = "site") %>% 
  mutate(
    aug_ratio = event_rainfall_total/historical_aug_rainfall_total,
    summary_at = now
  ) %>%
  st_drop_geometry() %>%
  select(-geometry) %>% 
  drop_na()

rainfall_out %>% write.csv("outputs/tots_rainfall_event_summary.csv")


p <- ggplot(rainfall_out, aes(longitude, latitude, z = event_rainfall_total)) +
  geom_point(aes(colour=event_rainfall_total, group = site)) + 
  stat_density2d()

ggplotly(p)
