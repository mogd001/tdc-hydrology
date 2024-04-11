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
library(scales)
library(DT)
library(crosstalk)
library(leaflet)
library(leafpop)
library(ggmap)

library(config)

config <- config::get()

# Topo template
t_prefix <- "http://tiles-a.data-cdn.linz.govt.nz/services;key="
linz_key <- config$linzkey
tMid <- "/tiles/v4/layer="
tSuffix <- "/EPSG:3857/{z}/{x}/{y}.png"
topo_50_template <- paste0(t_prefix, linz_key, tMid, 50767, tSuffix)

stadia_key <- config$stadiakey

# Stadia toner template
t_prefix <- "https://tiles.stadiamaps.com/tiles/stamen_toner/{z}/{x}/{y}{r}.png?api_key="
stadia_toner_template <-  paste0(t_prefix, stadia_key)

# Stadia toner lite template
t_prefix <- "https://tiles.stadiamaps.com/tiles/stamen_toner_lite/{z}/{x}/{y}{r}.png?api_key="
stadia_toner_lite_template <- paste0(t_prefix, stadia_key)


################################################################################
#
# Title: Rainfall Summary Past 24 Hours
# Author: Matt Ogden
# Description: This script summarises rainfall totals for the past 24 hours with a map output.
#
################################################################################

unlink("outputs/*", recursive = TRUE)

Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoibWF0dG9nZGVuIiwiYSI6ImNsNDNjY21jNDAxZmYzb3A4NWE2Y2M2cXEifQ.4vG7Zx9grk1z7R9LsDPeEA")
datatables_fp <- r"{\\tsrvfiles\hydrology\Datafiles\Data Tables}"

catchments <- catchments <- st_read("data/context.gpkg", layer = "catchments")

rivers <- st_read("data/context.gpkg", layer = "rivers") %>%
  st_transform(crs = 4326)

sites <- get_sites(collection = "AllRainfall", synonyms = TRUE) %>%
  mutate(
    longitude_ = longitude,
    latitude_ = latitude
  ) %>%
  st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
  st_transform(crs = 2193) %>%
  st_join(catchments, join = st_intersects) %>%
  replace_na(list(catchment = "Motueka"))

site_names <- select(sites, site, second_synonym) %>%
  st_drop_geometry() %>%
  rename(site_name = second_synonym)

# Get rainfall last 24 hours
n <- now()
from <- format(n - hours(24), "%Y%m%dT%H0000")
to <- format(n, "%Y%m%dT%H0000") 

rainfall <- get_data_collection(collection = "AllRainfall", method = "Total", interval = "1 hour", from = from, to = to) %>%
  distinct() %>%
  mutate(datetime = with_tz(datetime, tz = "NZ")) %>%
  left_join(site_names, by = "site") %>%
  mutate(rainfall_total_mm = round(value, 1)) %>%
  select(-value)

rainfall_p24hrs <- rainfall %>%
  group_by(site) %>%
  summarise(total_p24hrs = round(sum(rainfall_total_mm), 1)) %>%
  st_drop_geometry()

rescale_vec <- function(x, new_min, new_max) {
  ((x - min(x)) / (max(x) - min(x))) * (new_max - new_min) + new_min
}

max_rainfall <- max(rainfall_p24hrs$total_p24hrs, na.rm = TRUE)
max_datetime <- max(rainfall$datetime, na.rm = TRUE)

max_radius <- if (max_rainfall >= 200) {
  max_radius <- 50
} else if (max_rainfall >= 160) {
  max_radius <- 40
} else if (max_rainfall >= 120) {
  max_radius <- 30
} else if (max_rainfall >= 80) {
  max_radius <- 20
} else if (max_rainfall >= 40) {
  max_radius <- 10
} else {
  max_radius <- 5
}

rainfall_summary <- sites %>%
  st_drop_geometry() %>%
  right_join(rainfall_p24hrs, by = c("site" = "site")) %>%
  select(-c(site, first_synonym)) %>%
  rename(site_name = second_synonym) %>%
  relocate(site_name) %>%
  arrange(desc(total_p24hrs)) %>%
  mutate(radius = rescale_vec(total_p24hrs, 2, max_radius)) %>% # add extra column for plotting purposes
  relocate(radius, .after = total_p24hrs)

# https://emilyriederer.github.io/demo-crosstalk/tutorial/tutorial-rmd.html#Ex:_One_dataset,_two_widgets
rf_ct <- SharedData$new(rainfall_summary, key = ~site_name)

table_link <- "file:///M:/Datafiles/Data%20Tables/RainHour.html"

table <- rf_ct %>%
  datatable(
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left; color:black;  font-size:100% ;",
      htmltools::withTags(
        div(HTML(glue('<a href={table_link} target="_blank">Rainfall Past 24 Hours</a> {max(rainfall$datetime)}
                       <br>{format(min(rainfall$datetime), "%A %d %B %Y %I %p")} to {format(max(rainfall$datetime), "%A %d %B %Y %I %p")}
                      ')))
      )
    ),
    rownames = FALSE,
    colnames = c("Site", "Lat", "Long", "Catchment", "Total", "Radius"),
    # filter = list(position = "bottom"),
    options = list(
      searching = FALSE,
      pageLength = 15, lengthChange = FALSE, scrollX = TRUE, autoWidth = FALSE,
      columnDefs = list(
        list(targets = c(1, 2, 5), visible = FALSE)
      )
    )
  )

catchments <- st_transform(catchments, 4326)
catchment_centroids <- catchments %>% st_centroid(catchments)

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

generate_plot <- function(site_name) {
  print(site_name)
  r_filtered <- filter(rainfall, site_name == !!site_name)

  coeff <- 10
  breaks <- seq(min(r_filtered$datetime, na.rm = TRUE), max(r_filtered$datetime, na.rm = TRUE), by = "1 hours")
  date_labels <- c(sapply(breaks, function(x) {
    if (hour(x) == 0) {
      format(x, "%a %d %B-%H")
    } else {
      format(x, "%H")
    }
  }))

  p <- r_filtered %>%
    mutate(
      text = if_else(rainfall_total_mm == 0, "", as.character(rainfall_total_mm))
    ) %>%
    ggplot(aes(x = datetime - minutes(30), y = rainfall_total_mm)) +
    geom_col(color = "blue", fill = "blue", alpha = 0.4) +
    geom_text(aes(label = text), size = 2, color = "black", vjust = 2) +
    geom_line(aes(x = datetime, y = cumsum((rainfall_total_mm)) / coeff), size = 0.8, color = "magenta", linetype = "twodash", inherit.aes = FALSE) +
    scale_y_continuous(
      name = "Hourly Rainfall (mm)",
      limits = c(0, max(round_any(max(r_filtered$rainfall_total_mm), 10, ceiling), 10)),
      expand = c(0, 0),
      sec.axis = sec_axis(~ . * coeff, name = "Cumulative Rainfall (mm)"),
    ) +
    theme_bw() +
    labs(x = "Datetime (NZDT)", title = glue("{site_name} Rainfall"), subtitle = glue("Total {sum(r_filtered$rainfall_total_mm)} mm")) +
    scale_x_datetime(breaks = breaks, date_labels = date_labels) +
    theme(
      plot.subtitle = element_text(color = "magenta"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.ticks.y.left = element_line(colour = "blue"),
      axis.title.y.left = element_text(colour = "blue"),
      axis.line.y.left = element_line(color = "blue"),
      axis.text.y.left = element_text(color = "blue"),
      axis.ticks.y.right = element_line(colour = "magenta"),
      axis.title.y.right = element_text(colour = "magenta", angle = 90),
      axis.line.y.right = element_line(color = "magenta"),
      axis.text.y.right = element_text(color = "magenta")
    )

  return(p)
}

p_all <- lapply(rainfall_summary$site_name, generate_plot)

map <- leaflet(height = 900) %>%
  addTiles(urlTemplate = topo_50_template, group = "NZ Topo50") %>%
  addTiles(group = "OSM (default)") %>%
  addTiles(urlTemplate = stadia_toner_template, group = "Toner") %>%
  addTiles(urlTemplate = stadia_toner_lite_template, group = "Toner Lite") %>%
  # add catchments
  addPolygons(
    group = "Catchments",
    data = catchments,
    fillColor = "blue",
    weight = 2,
    opacity = 1,
    color = "blue",
    fillOpacity = 0.1,
    labelOptions = labelOptions(noHide = FALSE, direction = "auto")
  ) %>%
  addLabelOnlyMarkers(
    group = "Catchments",
    data = catchment_centroids,
    label = ~ as.character(catchment),
    labelOptions = labelOptions(
      noHide = TRUE, direction = "top", textOnly = TRUE,
      style = list(
        "color" = "blue",
        "font-family" = "serif",
        "font-style" = "bold",
        # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
        "font-size" = "15px"
        # "border-color" = "rgba(0,0,0,0.5)"
      )
    )
  ) %>%
  # add rivers
  addPolylines(
    group = "Rivers",
    data = rivers,
    weight = 1
  ) %>%
  # add sites
  addCircleMarkers(
    group = "Sites",
    data = rf_ct,
    radius = ~radius,
    color = "blue",
    stroke = TRUE,
    fillOpacity = 0.6,
    label = ~ paste0(as.character(site_name), ": ", total_p24hrs, " mm"),
    # clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
    labelOptions = labelOptions(noHide = FALSE, direction = "auto", style = list(
      "color" = "black",
      "font-family" = "serif",
      "font-size" = "15px"
    ))
  ) %>%
  addPopupGraphs(p_all, group = "Sites", width = 600, height = 500, dpi = 300) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Toner Lite", "NZ Topo50", "OSM (default)", "Toner"),
    overlayGroups = c("Catchments", "Rivers", "Sites"),
    options = layersControlOptions(collapsed = TRUE)
  )

p <- crosstalk::bscols(
  widths = c(4, 8),
  div(table, style = css(width = "100%", height = "100%")),
  div(map, style = css(width = "100%", height = "100%"))
)

# create folder outputs
dir.create("outputs")

htmltools::save_html(p, file = glue("outputs/{format(max_datetime, '%Y%m%d-%H')}_rainfall_summary_p24hrs.html"))

# Upload to sharepoint
# library(Microsoft365R)
# site <- get_sharepoint_site(site_name = "Environmental Monitoring")
# site$get_drive("Reports and Analyses")$upload_file(glue("outputs/{format(max_datetime, '%Y%m%d-%H')}_rainfall_summary_p24hrs.html"), glue("R Outputs/rainfall_summary_p24hrs.html"))
# 
# tryCatch(
#   expr = {
#     site$get_drive("Reports and Analyses")$delete_item("R Outputs/lib/image-Sites-0.0.1")
#   },
#   error = function(e) {
#     print("No image-Sites folder to delete.")
#   }
# )
# 
# upload_image_sharepoint <- function(x, target_folder) {
#   site$get_drive("Reports and Analyses")$upload_file(glue("outputs/{target_folder}/{x}"), glue("R Outputs/{target_folder}/{x}"))
# }
# 
# sites_files <- list.files("outputs/lib/image-Sites-0.0.1")
# map(sites_files, upload_image_sharepoint, target_folder = "lib/image-Sites-0.0.1")