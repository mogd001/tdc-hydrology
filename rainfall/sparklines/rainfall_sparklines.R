library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)
library(sf)
library(plotly)
library(scales)
library(htmltools)
library(htmlwidgets)

unlink("rainfall-sparklines/*", recursive = TRUE)

catchments <- catchments <- st_read("data/catchments.gpkg", layer = "catchments") %>%
  mutate(catchment = factor(catchment,
    ordered = TRUE,
    levels = c("Aorere", "Takaka", "Riwaka", "Motueka", "Marahau", "Moutere", "Waimea", "Nelson", "Buller", "Other")
  ))

sites <- get_sites(collection = "AllRainfall") %>%
  mutate(
    longitude_ = longitude,
    latitude_ = latitude
  ) %>%
  st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
  st_transform(crs = 2193) %>%
  st_join(catchments, join = st_intersects) %>%
  replace_na(list(catchment = "Motueka"))

site_catchment <- sites %>%
  st_set_geometry(NULL) %>%
  select(site, catchment)

rainfall_data <- get_data_collection(collection = "AllRainfall", method = "Total", interval = "1 hour", time_interval = "P7D") %>%
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

# Create labelleler to modify facet wraps
catchment_names <- lapply(catchments$catchment, function(x) "")
names(catchment_names) <- catchments$catchment

site_names <- unique(rainfall_data$site)
names(site_names) <- site_names

plot_labeller <- function(variable, value) {
  if (variable == "catchment") {
    return(as.character(catchment_names[value]))
  } else {
    return(as.character(site_names[value]))
  }
}

rainfall_data_p7d_highlight <- rainfall_data_p7d %>%
  rename(dt = datetime, r = rainfall) %>%
  mutate(r = max(r))

# ggplot
p <- ggplot() +
  geom_col(rainfall_data_p7d, mapping = aes(x = datetime, y = rainfall, fill = catchment)) +
  facet_wrap(catchment ~ site, scales = "free_y", labeller = plot_labeller) +
  labs(y = NULL, x = NULL, fill = "Catchment", title = glue("Rainfall P7D (mm) {min_datetime} - {max_datetime}  [NZDT]")) +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 1),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

ggsave(glue("rainfall-sparklines/{as.Date(max_datetime, tz = 'NZ')}_rainfall_sparklines.png"), p, dpi = 300, height = 10, width = 19)

# ggplotly
p <- ggplot() +
  geom_line(rainfall_data_p7d_highlight, mapping = aes(x = dt, y = r, frame = date), size = 2, color = "red", alpha = 0.2) +
  geom_col(rainfall_data_p7d, mapping = aes(x = datetime, y = rainfall, fill = catchment, label = site)) +
  facet_wrap(catchment ~ site, scales = "free_y", labeller = plot_labeller) +
  labs(y = NULL, x = NULL, fill = "Catchment", title = glue("Rainfall P7D (mm) {min_datetime} - {max_datetime}  [NZDT]")) +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 1),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )


plotly_p <- ggplotly(p, tooltip = c("datetime", "rainfall", "site"), height = 1000, width = 1800) %>%
  layout(hovermode = "x") %>%
  config(labeller = as_labeller(plot_labeller)) %>%
  layout(hovermode = "x unified") %>%
  animation_opts(easing = "elastic") %>%
  animation_button(visible = FALSE)

save_html(plotly_p, file = glue("rainfall-sparklines/{as.Date(max_datetime, tz = 'NZ')}_rainfall_sparklines.html"))

# Upload to sharepoint
library(Microsoft365R)
site <- get_sharepoint_site(site_name = "Environmental Monitoring")
site$get_drive("Reports and Analyses")$upload_file(glue("rainfall-sparklines/{as.Date(max_datetime, tz = 'NZ')}_rainfall_sparklines.png"), glue("R Outputs/rainfall_sparklines.png"))
