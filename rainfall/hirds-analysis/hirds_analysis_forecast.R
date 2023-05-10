library(tdcR)
library(tidyverse)
library(lubridate)
require(scales)
library(plotly)
library(htmlwidgets)
library(glue)
library(knitr)
library(kableExtra)
library(purrr)
library(ggmap)
library(sf)
library(viridis)

source("functions.R")

sites <- get_sites(collection = "AllRainfall")
from <- "Now"
to <- "Data end"
forecast_start <- now() 

dt_frmt <- "%A %d %B %Y %I%p"
  
nelsontasman <- st_read("data/context.gpkg", layer = "nelsontasman")
nelsontasman_wgs84 <- st_transform(nelsontasman, crs = 4326)
context <- st_read("data/context.gpkg", layer = "towns", query = "SELECT * FROM \"towns\" WHERE name <> 'Brightwater'")
catchments <- st_read("data/context.gpkg", layer = "catchments")
catchments_wgs84 <- st_transform(catchments, crs = 4326)
context <- st_read("data/context.gpkg", layer = "towns", query = "SELECT * FROM \"towns\" WHERE name <> 'Brightwater'")
context_wgs84 <- st_transform(context, crs = 4326) %>%
  mutate(
    lon = st_coordinates(.)[, "X"],
    lat = st_coordinates(.)[, "Y"]
  )

#test
#t <- get_max_rainfall(site, "1 hour", "Now", "Data end", endpoint = "http://envdata.tasman.govt.nz/forecasts.hts?", measurement = "Forecast Rainfall")
# consider future rainfall
duraction_text_to_val <- tribble(
  ~duration_text, ~duration, ~duration_interval, ~duration_label,
  #  "dur_10min", 1 / 6, "10 minutes", "10 min",
  #  "dur_20min", 2 / 6, "20 minutes", "20 min",
  #  "dur_30min", 3 / 6, "30 minutes", "30 min",
  "dur_1hour", 1, "1 hour", "1 hour",
  "dur_2hour", 2, "2 hours", "2 hours",
  "dur_6hour", 6, "6 hours", "6 hours",
  "dur_12hour", 12, "12 hours", "12 hours",
  "dur_1day", 24, "24 hours", "1 day",
  "dur_2day", 48, "48 hours", "2 days" #,
  #  "dur_3day", 72, "72 hours", "3 days",
  #  "dur_4day", 96, "96 hours", "4 days",
  #  "dur_5day", 120, "120 hours", "5 days"
)

nz_record <- tribble(
  ~duration, ~value,
  1/6, 34,
  1, 134,
  12, 566,
  24, 758,
  48, 1086,
  120, 1368
)

nelson_tasman_record <- tribble(
  ~duration, ~duration_label, ~value, ~site , ~event,
  1 / 6, "10 min", 32, "HY Roding at Caretakers", "April 2013",
  3 / 6, "30 min", 62, "HY Roding at Caretakers", "April 2013",
  1, "1 hour", 104, "HY Roding at Caretakers", "April 2013",
  2, "2 hours", 138.5, "HY Waimea at TDC Nursery", "April 2013",
  6, "6 hours", 218.4, "HY Takaka at Harwoods", "February 2018",
  12, "12 hours",  343.1, "HY Anatoki at Paradise", "February 2016",
  24, "1 day", 493.5, "HY Bainham at Langfords Store", "April 1931",
  48, "2 days", 742.8, "HY Anatoki at Paradise", "August 2022",
  72, "3 days", 834.7, "HY Anatoki at Paradise", "August 2022",
  120, "5 days", 1058, "HY Anatoki at Paradise", "August 2022"
)


ari_bins <- c(-999, 1.58, 2, 5, 10, 20, 50, 100, 9999)
ari_bin_labels <- c("<1.58", "1.5-2", "2-5", "5-10", "10-20", "20-50", "50-100", ">100")

# Analyse HIRDs by site
analyse_hirds_site <- function(site) {
  
  try ({
    forecast_rainfall <- tdcR::get_data_site_measurement(endpoint = "http://envdata.tasman.govt.nz/forecasts.hts?", site = site, measurement = "Forecast Rainfall", from = "Now", to = "Data end")
    min_forecast_datetime <- min(forecast_rainfall$datetime) %>% with_tz(tz = "NZ") 
    max_forecast_datetime <- max(forecast_rainfall$datetime) %>% with_tz(tz = "NZ") 
    
    site_information <- tdcR::get_site_information_envmon(site)
    hirds_table <- tdcR::tabulate_hirds_data_string(site_information$hirds_data)
    
    hirds <- hirds_table %>%
      mutate(ari = fct_rev(factor(ari))) %>%
      pivot_longer(!ari, names_to = "duration_text", values_to = "val") %>%
      left_join(duraction_text_to_val, by = "duration_text") %>%
      filter(ari %in% ari_bins)
    
    intervals <- duraction_text_to_val$duration_interval
    
    forecast_hirds <- do.call(rbind, lapply(intervals, get_max_rainfall, site = site, from = from, to = to, endpoint = "http://envdata.tasman.govt.nz/forecasts.hts?", measurement = "Forecast Rainfall")) %>%
      left_join(duraction_text_to_val, by = "duration_interval") %>%
      mutate(
        ari_for_duration_val = unlist(map2(duration_label, value, determine_ari_duration, hirds)),
        ari_for_duration = if_else(ari_for_duration_val == -999, "<1.58", 
                                   if_else(ari_for_duration_val == 999, ">100+", as.character(round(ari_for_duration_val, 1)))),
        ari_for_duration_bin = factor(ari_bin_labels[findInterval(ari_for_duration_val, ari_bins, all.inside=TRUE)], levels = ari_bin_labels, ordered = TRUE)
      )
  
    dplyr::select(forecast_hirds, c(duration_label, rainfall_mm = value, ari_for_duration)) %>% 
      mutate(rainfall_mm = round(rainfall_mm, 1)) %>% 
      knitr::kable("html", col.names = c("Duration",  "Rainfall (mm)", "ARI")) %>% 
      kable_styling("striped") %>%
      save_kable("temp_table.html")
    
    # Plot 1
    arrow_nz <- tibble(x1 = c(12), x2 = c(23.5), y1 = c(2000), y2 = c(820))
    arrow_nt <- tibble(x1 = c(1/2), x2 = c(5/6), y1 = c(400), y2 = c(95))
    
    p_hirds_site <- ggplot() +
      geom_line(hirds, mapping = aes(duration, val, color = ari), linetype = "dashed") +
      
      geom_line(nz_record, mapping = aes(duration, value), color = "red", size = 1, alpha = 0.5) +
      geom_point(nz_record, mapping = aes(duration, value), color = "red", size = 2) +
      geom_line(nelson_tasman_record, mapping = aes(duration, value), color = "orange", size = 1, alpha = 0.5) +
      geom_point(nelson_tasman_record, mapping = aes(duration, value), color = "orange", size = 2) +
      
      geom_text(nelson_tasman_record, mapping = aes(duration, value, label = glue("{substring(site, 4)} {event}")), color = "orange", size = 2, angle = 75, hjust = -0.1, vjust = 0) + 
      
      geom_curve(
        data = arrow_nz, aes(x = x1, y = y1, xend = x2, yend = y2),
        size = 0.5,
        arrow = arrow(length = unit(0.08, "inch")),
        color = "red", curvature = -0.1) +
      geom_label(aes(x = 12, y = 2000, label = "NZ Record"), color = "red", fill = "white") +
      
      geom_curve(
        data = arrow_nt, aes(x = x1, y = y1, xend = x2, yend = y2),
        size = 0.5,
        arrow = arrow(length = unit(0.08, "inch")),
        color = "orange", curvature = -0.2) +
      geom_label(aes(x = 1/2, y = 400, label = "Nelson Tasman Record"), color = "orange", fill = "white") +
      
      geom_line(forecast_hirds, mapping = aes(duration, value), color = "magenta", size = 1) +
      geom_point(forecast_hirds, mapping = aes(duration, value), color = "magenta", size = 2.5) +
      
      scale_x_continuous(trans = "log10", breaks = duraction_text_to_val$duration, labels = duraction_text_to_val$duration_label) +
      scale_y_log_eng(limits = c(7, 2900)) +
      scale_color_viridis_d() +
      labs(
        x = "Duration", y = "Rainfall (mm)", color = "ARI (years)", linetype = NULL,
        title = glue(
          {site},
          " Forecast Rainfall Hirds Analysis"
        ),
        subtitle = glue(        
          {format(min_forecast_datetime , dt_frmt)},
          " to ",
          {format(max_forecast_datetime , dt_frmt)}
        )
      ) +
      annotation_logticks(sides = "l") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(colour = "magenta"),
        panel.grid.minor.x = element_blank()
      )
    
    ggsave(glue("outputs/forecast/{site}_hirds_forecast_output.png"), plot = p_hirds_site, dpi = 300, height = 8, width = 12)
    
    forecast_hirds %>% 
      mutate(site = site,
             min_forecast_datetime = min_forecast_datetime,
             max_forecast_datetime = max_forecast_datetime) 
  })
}

res <- lapply(sites$site, analyse_hirds_site)

res <- res %>% # remove sites where hirds analysis could not be performed
  keep(is_tibble)

hirds_results <- dplyr::bind_rows(res) %>% 
  left_join(sites, by = "site")

# Map results
generate_hirds_forecast_plot <- function(d) {
  
  # 2 day duration
  hirds_dur <- hirds_results %>% 
    filter(duration == d)
  
  basemap <- get_map(location = c(lon = 172.83031417, lat = -41.40166015), maptype = "terrain-background", zoom = 8, alpha = 0.3) # Richmond, NZ (alternative for getting location)
  bb <- st_bbox(nelsontasman_wgs84)
  
  
  # p1 should show the rainfall totals
  p1 <- ggmap(basemap, darken = c(0.6, "white")) +
    coord_cartesian() +
    geom_sf(nelsontasman_wgs84, mapping = aes(), fill = NA, color = "white", inherit.aes = FALSE) + # note: geom_sf sets to correct aspect ratio
    geom_sf(context_wgs84, mapping = aes(), fill = NA, shape = 2, size = 2.5, color = "black", inherit.aes = FALSE) +
    geom_text(context_wgs84, mapping = aes(lon, lat, label = name), size = 3.5, color = "black", hjust = -0.18, fontface = "bold") +
    geom_point(hirds_dur, mapping = aes(x = longitude, y = latitude, size = value, color = value)) +
    geom_text(hirds_dur, mapping = aes(x = longitude, y = latitude, label = value), fontface = "bold", color = "white", size = 1.75, alpha = 1.0) +
    coord_sf(xlim = c(bb$xmin - 0.3, bb$xmax + 0.2), ylim = c(bb$ymin - 0.2, bb$ymax + 0.2)) +
    labs(color = "Rainfall Total (mm)", title = glue("Peak rainfall for duration")) +
    theme_bw() +
    scale_color_viridis(option = "turbo", limits = c(0, 500), alpha = 0.7) +
    scale_size_continuous(range = c(4, 8)) +
    guides(size = "none") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_blank(),
      legend.position = c(0.15, 0.9)
    )
  
  # p2 should show the computed hirds at each site for the duration
  
  p2 <- ggmap(basemap, darken = c(0.6, "white")) +
    coord_cartesian() +
    geom_sf(nelsontasman_wgs84, mapping = aes(), fill = NA, color = "white", inherit.aes = FALSE) +
    geom_sf(catchments_wgs84, mapping = aes(), fill = NA, color = "white", inherit.aes = FALSE) +
    geom_sf(context_wgs84, mapping = aes(), fill = NA, shape = 2, size = 2.5, color = "black", inherit.aes = FALSE) +
    geom_text(context_wgs84, mapping = aes(lon, lat, label = name), size = 3.5, color = "black", hjust = -0.18, fontface = "bold") +
    geom_point(hirds_dur, mapping = aes(x = longitude, y = latitude, color = ari_for_duration_bin), size = 10) +
    geom_text(hirds_dur, mapping = aes(x = longitude, y = latitude, label = ari_for_duration_bin), fontface = "bold", color = "white", size = 1.75, alpha = 1.0) +
    coord_sf(xlim = c(bb$xmin - 0.3, bb$xmax + 0.2), ylim = c(bb$ymin - 0.2, bb$ymax + 0.2)) +
    #scale_color_manual(values = c("red", "blue", "green", "purple", "pink", "yellow", "orange", "blue"),
    #                   labels = ari_bin_labels, 
    #                   drop = FALSE) +
    scale_color_viridis_d(labels = ari_bin_labels, drop = FALSE) +
    labs(color = "ARI", title = glue("HIRDs for duration")) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_blank(),
      legend.position = c(0.15, 0.78),
      legend.key = element_blank()
    )
  
  theme_border <- theme_gray() +
    theme(
      plot.background = element_rect(fill = NA, colour = "#273691", size = 3),
      plot.title = element_text(size = 20, hjust = 0.5, vjust = -0.75, face = "bold", colour = "#273691"),
      plot.caption = element_text(size = 7, face = "italic", color = "black")
    )
  
  library(patchwork)
  p_out <- p1 + p2 +
    plot_annotation(
      title = glue('{d} Hours HIRDs'),
      subtitle = glue('{format(min(hirds_dur$min_forecast_datetime), dt_frmt)} - {format(min(hirds_dur$max_forecast_datetime), dt_frmt)}'),
      caption = glue("TDC Environmental Data"), # compiled {now_plot}
      theme = theme_border
    )
  
  ggsave(glue('outputs/hirds_forecast_{d}_hours.png'), width = 12, height = 9.6, plot = p_out, dpi = 300) 
}

durations <- duraction_text_to_val$duration
generate_hirds_forecast_plot(durations[4])
lapply(durations, generate_hirds_forecast_plot)
