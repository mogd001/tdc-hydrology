library(tidyverse)
library(XML)
library(rvest)
library(lubridate)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(tdcR)
library(sf)
library(glue)

################################################################################
# 
# Title: Rainfall Summary Past 7 Days
# Author: Matt Ogden
# Description: Report on rainfall from the last 7 days.
#
################################################################################

unlink("outputs/*", recursive = TRUE)

dt_frmt <- "%A %d %B %Y %I %p"

catchments <- st_read("data/catchments.gpkg", layer = "catchments") %>%
  mutate(catchment = factor(catchment,
                            ordered = TRUE,
                            levels = c("Aorere", "Takaka", "Riwaka", "Motueka", "Marahau", "Moutere", "Waimea", "Nelson", "Buller", "Other")
  ))

sites <- get_sites(collection = "AllRainfall", synonyms = TRUE) %>%
  mutate(
    longitude_ = longitude,
    latitude_ = latitude,
    site_name = second_synonym 
  ) %>%
  st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
  st_transform(crs = 2193) %>%
  st_join(catchments, join = st_intersects) %>%
  replace_na(list(catchment = "Motueka"))

site_catchment <- sites %>%
  st_set_geometry(NULL) %>%
  select(site, site_name, catchment)

n <- now()
from <- format(n - days(7), "%Y%m%dT%H0000")
to <- format(n, "%Y%m%dT%H0000") 

rainfall_data <- get_data_collection(collection = "AllRainfall", method = "Total", interval = "1 hour", from = from, to = to) %>%
  rename(rainfall = value) %>%
  mutate(
    datetime = with_tz(datetime, "NZ"),
    date = as.numeric(format(as.Date(datetime, t = "NZ"), "%Y%m%d")),
    rainfall = round(rainfall, 2)
  ) %>%
  left_join(site_catchment, by = "site")

max_datetime <- max(rainfall_data$datetime)
min_datetime <- max_datetime - days(7)

rainfall_data_p7d <- rainfall_data %>%
  filter(datetime >= min_datetime)

r_summary <- rainfall_data_p7d  %>%
  group_by(site) %>%
  summarise(
    p7d_rainfall_total = round(sum(rainfall, na.rm = TRUE), 0),
    p7d_max_hrly_rainfall = round(max(rainfall, na.rm = TRUE), 0)
  ) %>% 
  left_join(select(sites, c(site, catchment, site_name)), by = "site")

p <- ggplot(r_summary, aes(x = reorder(site_name, -p7d_rainfall_total), y = p7d_rainfall_total, fill = catchment, 
                           text = paste("Site:", site_name, "\n 7 Day Rainfall Total:", p7d_rainfall_total, "mm"))) +
  geom_bar(color = "black", alpha = 0.6, stat = "identity") +
  geom_text(mapping = aes(label = p7d_rainfall_total), size = 2, vjust = -1) + 
  theme_bw() +
  labs(x = "", y = "Rainfall Total (mm)", fill = "Catchment", title = glue("Rainfall Past 7 days (mm) from {format(min_datetime, dt_frmt)} to {format(max_datetime, dt_frmt)}  [NZDT]")) + #caption = glue("at {now_plot})"
  scale_y_continuous(limits = c(0, max(r_summary$p7d_rainfall_total * 1.05)), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(glue("outputs/{format(max_datetime, '%Y%m%d-%H')}_rainfall_summary_p7days.png"), p, dpi = 300, height = 10, width = 16)

p2 <- ggplot(r_summary, aes(x = reorder(site_name, -p7d_rainfall_total), y = p7d_rainfall_total, fill = catchment, 
                           text = paste("Site:", site_name, "\n 7 Day Rainfall Total:", p7d_rainfall_total, "mm"))) +
  geom_bar(color = "black", alpha = 0.6, stat = "identity") +
  theme_bw() +
  labs(x = "", y = "Rainfall Total (mm)", fill = "Catchment", title = glue("Rainfall Past 7 days (mm) from {format(min_datetime, dt_frmt)} to {format(max_datetime, dt_frmt)}  [NZDT]")) + #caption = glue("at {now_plot})"
  scale_y_continuous(limits = c(0, max(r_summary$p7d_rainfall_total * 1.05)), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plotly_p <- ggplotly(p2, tooltip = c("text"), height = 900, width = 1700)
save_html(plotly_p, file = glue("outputs/{format(max_datetime, '%Y%m%d-%H')}_rainfall_summary_p7days.html"))

# Upload to sharepoint
# library(Microsoft365R)
# site <- get_sharepoint_site(site_name = "Environmental Monitoring")
# site$get_drive("Reports and Analyses")$upload_file(glue("outputs/{format(max_datetime, '%Y%m%d-%H')}_rainfall_summary_p7days.html"), glue("R Outputs/rainfall_summary_p7days.html"))
