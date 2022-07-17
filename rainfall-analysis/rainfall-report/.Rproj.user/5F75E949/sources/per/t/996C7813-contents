library(tidyverse)
library(lubridate)
library(Hilltop)
library(hillr)
library(plotly)
library(RODBC)
library(scales)
library(XML)
library(xml2)
library(httr)
# This script aims to replicate the webpage below.
# https://www.tasman.govt.nz/my-region/environment/environmental-data/rainfall/anatoki-at-happy-sams/

source("rainfall_functions.R")

endpoint <- "http://envdata.tasman.govt.nz/data.hts?"

catchment <- unlist(strsplit(site, " "))[2]
# Need to replace with actual data
altitude <- "70m"
analysis_period <- "31 October 1990 to 31 December 2019."
exposure <-
  "A narrow valley in the low level foothills, surrounded by regenerating bush."

# Get site information and prepare data
all_sites <- getHilltopSites(endpoint = endpoint)
s_filter <- as_vector(grepl(site, all_sites$site))
site_information <- all_sites %>%
  filter(s_filter) %>%
  rename_all(tolower)

envmon_site_information <- read_site_information(site)
site_name <- envmon_site_information$first_synonym

# Load timeseries data
end_date <- floor_date(now(), "1 hour")
start_date <- today() %m+% months(-3) # 3 months before end date

df <-
  tibble(datetime = seq(ymd_hms(paste(
    start_date, "00:00:00", sep = " "
  )), ymd_hms(paste(
    as.Date(end_date),
    format(as.POSIXct(end_date), format = "%H:%M:%S"),
    sep = " "
  )), by = "1 hour"))
df$datetime <-
  as.POSIXct(format(df$datetime), tz = "") # remove timezone to ensure subsequent join is done correctly

rainfall_df <- getHilltopData(
  endpoint = endpoint,
  site = site,
  measurement = measurement,
  from = format(start_date, format = "%d/%m/%Y"),
  to = ""
) %>%
  as_tibble() %>%
  rename_with(tolower) %>%
  rename(datetime = time, rainfall = value) %>%
  mutate(date_hour = floor_date(datetime, "1 hour"),
         rainfall = as.double(rainfall)) %>%
  group_by(date_hour) %>%
  summarize(rainfall = sum(rainfall, na.rm = FALSE)) %>%
  rename(datetime = date_hour)

rainfall_df <- df %>%
  left_join(rainfall_df, by = "datetime") %>%
  mutate(date = as.Date(datetime, tz = "NZ")) %>%
  replace_na(list(rainfall = 0))

hrly_rainfall_df <- rainfall_df %>%
  filter(datetime > max(datetime) - days(7)) # last 7 days
total_hrly <- sum(hrly_rainfall_df$rainfall, na.rm = FALSE)

daily_rainfall_df <- rainfall_df %>%
  group_by(date = floor_date(datetime, "1 day")) %>%
  summarize(rainfall = sum(rainfall, na.rm = FALSE)) %>%
  filter(date > max(date) %m+% months(-1)) %>% # last 1 month
  mutate(date = as.Date(date))
total_daily <- sum(daily_rainfall_df$rainfall, na.rm = TRUE)

# Plots and table
y_limit <- max(hrly_rainfall_df$rainfall) * 1.2
hrly_plot <- ggplot(hrly_rainfall_df, aes(datetime, rainfall)) +
  geom_step(color = "red") +
  scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%b-%d-%Y ")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, y_limit)) +
  theme(
    panel.background = element_rect(
      fill = "white",
      colour = "black",
      linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5,
      linetype = "dashed",
      colour = "grey"
    ),
    panel.border = element_rect(colour = "black", fill = NA),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(x = "",
       y = "Rainfall (mm)",
       title = paste0("Total= ", total_hrly, " mm"))

y_limit <- round_any(max(daily_rainfall_df$rainfall) * 1.2, 10)
daily_plot <- ggplot(daily_rainfall_df, aes(date, rainfall)) +
  geom_step(color = "red") +
  scale_x_date(breaks = date_breaks("1 week"), labels = date_format("%b-%d-%Y ")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, y_limit)) +
  theme(
    panel.background = element_rect(
      fill = "white",
      colour = "black",
      linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5,
      linetype = "dashed",
      colour = "grey"
    ),
    panel.grid.minor = element_line(
      size = 0.25,
      linetype = "dashed",
      colour = "grey"
    ),
    panel.border = element_rect(colour = "black", fill = NA),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "Rainfall (mm)",
    title = paste0("Total= ", total_daily, " mm")
  )

hirds_table <-
  tabulate_hirds_data_string(envmon_site_information$hirds_data) %>%
  select(ARI, `1 hour`, `6 hour`, `1 day`) %>%
  filter(ARI %in% c(2, 5, 10, 20, 50, 100)) %>%
  rename(
    Period = ARI,
    `1 hr` = `1 hour`,
    `6 hour` = `6 hour`,
    `24 hr` = `1 day`
  )

# Modify period for table output
modify_period <- function(period) {
  return(paste0(as.character(period), " year"))
}
hirds_table$Period <- sapply(hirds_table$Period, modify_period)

map <- layout(
  plot_mapbox(site_information) %>%
    add_markers(
      x = ~ longitude,
      y = ~ latitude,
      text = ~ site
    ),
  mapbox = list(
    style = "basic",
    center = list(
      lat = ~ median(latitude),
      lon = ~ median(longitude)
    ),
    zoom = 7.8
  )
)
