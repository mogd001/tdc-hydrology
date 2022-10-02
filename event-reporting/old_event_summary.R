library(tidyverse)
library(lubridate)
library(plotly)
library(glue)
library(tdcR)
library(sf)
library(scales)
library(zeallot)
library(s2)
library(patchwork)
library(grid)
library(gridExtra)

get_flow_data <- function(from = format(default_start_date, "%Y%m%d"), to = "") {
  get_data_collection(
    collection = "ActiveFlowSites", method = "Extrema",
    time_interval = NA, from = from, to = to, interval = "15 minutes", alignment = "00:00"
  ) %>%
    rename(flow = value) %>%
    group_by(site) %>%
    slice(-1) %>%
    ungroup()
}

get_rainfall_data <- function(from = format(default_start_date, "%Y%m%d"), to = "") {
  get_data_collection(collection = "Rainfall", method = "Total", time_interval = NA, from = from, to = to, interval = "1 hour", alignment = "00:00") %>%
    rename(rainfall_total = value) %>%
    group_by(site) %>%
    arrange(site, datetime) %>%
    # slice(-1) %>% # remove first row due to offset, datetime refers to start of interval.
    ungroup() %>%
    mutate(
      interval = interval_dt,
      datetime = datetime + interval, # totals with rainfall so add one hour, "rainfall up to hour"
      rainfall_total = round(rainfall_total, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total))
}

default_start_date <- ymd("2022-08-16")
interval_dt <- hours(1)

now <- format(Sys.time(), "%Y%m%dT%H%M%S")
now_plot <- str_replace(now, "T", " ")

#flows <- get_flow_data(to = "Now")
#rainfall <- get_rainfall_data(to = "Now")

flows <- get_flow_data(to = "20220821")
rainfall <- get_rainfall_data(to = "20220821")

unreliable_rainfall_sites <- c("HY Nelson at Founders Park")

catchments <- st_read("data/catchments.gpkg", layer = "catchments_extended") %>% 
  mutate(catchment = factor(catchment,
                            ordered = TRUE,
                            levels=c("Aorere", "Takaka", "Riwaka", "Motueka", "Marahau", "Moutere", "Waimea", "Nelson", "Buller", "Other")))

sites <- get_sites() %>%
  mutate(
    longitude_ = longitude,
    latitude_ = latitude
  ) %>%
  st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
  st_transform(crs = 2193) %>%
  st_join(catchments, join = st_intersects) %>%
  replace_na(list(catchment = "Motueka"))

flow_thresholds <- read_csv("data/20220818_flow_thresholds.csv")

# Rainfall
rainfall_sites <- tibble(site = unique(rainfall$site))
rainfall_event_summary <- rainfall %>%
  group_by(site) %>%
  summarise(
    event_rainfall_total = round(sum(rainfall_total, na.rm = TRUE), 0),
    event_max_hrly_rainfall = round(max(rainfall_total, na.rm = TRUE), 0)
  )

rainfall_out <- rainfall_sites %>%
  left_join(sites, by = "site") %>%
  left_join(rainfall_event_summary, by = "site") %>%
  mutate(
    summary_at = now
  ) %>%
  st_drop_geometry() %>%
  select(-geometry)
rainfall_out %>% write.csv("outputs/rainfall_event_summary.csv")

# Flow
flow_sites <- tibble(site = unique(flows$site))
flow_event_summary <- flows %>%
  group_by(site) %>%
  summarise(
    event_flow_min = min(flow, na.rm = TRUE),
    event_flow_max = max(flow, na.rm = TRUE),
    event_flow_increase_percent = round((event_flow_max - event_flow_min) / event_flow_min, 1)
  )

flow_out <- flow_sites %>%
  left_join(sites, by = "site") %>%
  left_join(flow_event_summary, by = "site") %>%
  mutate(
    summary_at = now
  ) %>%
  st_drop_geometry() %>%
  select(-geometry)
flow_out %>% write.csv("outputs/flow_event_summary.csv")

# Plot rainfall totals by site
plot_rainfall <- left_join(rainfall_event_summary, sites, by = "site") %>% 
  mutate(site = substring(site, 4)) %>% 
  ggplot(aes(x = reorder(site, -event_rainfall_total), y = event_rainfall_total, fill = catchment)) +
  geom_bar(color = "black", alpha = 0.6, stat = "identity") +
  theme_bw() +
  labs(x = "", y = "Rainfall Total (mm)", fill = "Catchment", title = "Rainfall Total by Site 16-20 August 2022") + #caption = glue("at {now_plot})"
  scale_y_continuous(limits = c(0, max(rainfall_event_summary$event_rainfall_total * 1.05)), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
plot_rainfall

ggsave(glue("outputs/Rainfall_Totals.jpeg"), width = 10, height = 6)

htmlwidgets::saveWidget(ggplotly(plot_rainfall), glue("outputs/rainfall.html"))

# Plot rainfall intensity over time
plot_rainfall_intensity <- left_join(rainfall, sites, by = "site") %>%
  filter(catchment != "Other") %>% 
  rename(rainfall_intensity = rainfall_total) %>%
  ggplot(aes(x = datetime, y = rainfall_intensity, color = catchment, group = site)) +
  geom_step(alpha = 0.6) +
  theme_bw() +
  labs(x = "", y = "Hourly Rainfall (mm/hr)", title = "Rainfall Intensities by Catchment 16-20 August 2022") +
  scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(flows$datetime, na.rm = TRUE), by = "24 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="none") + 
  facet_wrap(~catchment, ncol = 8) # scales = "free_y"
plot_rainfall_intensity

ggsave(glue("outputs/Rainfall_Intensities.jpeg"), width = 20, height = 6)
htmlwidgets::saveWidget(ggplotly(plot_rainfall_intensity), glue("outputs/rainfall_intensities.html"))

# Plot flows over time
plot_flow <- ggplot(left_join(flows, sites, by = "site"), aes(x = datetime, y = flow, group = site, color = catchment)) +
  geom_line() +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", color = "Catchment", title = "Flows by Catchment 16-20 August 2022") +
  scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(flows$datetime, na.rm = TRUE), by = "24 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="none") + 
  facet_wrap(~catchment, scales = "free_y", ncol = 1,)
plot_flow

ggsave(glue("outputs/Flows.jpeg"), width = 10, height = 20)
htmlwidgets::saveWidget(ggplotly(plot_flow), glue("outputs/flows.html"))

# Plot local Nelson flows over time
plot_nelson_flows <- left_join(flows, sites, by = "site") %>%
  mutate(site = substring(site, 4)) %>% 
  filter(catchment == "Nelson" & site != "HY Orphanage at Ngawhatu") %>%
  ggplot(aes(x = datetime, y = flow, color = site)) +
  geom_line() +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", color = "Site", title = "Nelson") +
  scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(flows$datetime, na.rm = TRUE), by = "12 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(0, 500), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(glue("outputs/catchment_flows/Nelson.jpeg"), width = 10, height = 6)

# Plot local Waimea flows over time
plot_waimea_flows <- left_join(flows, sites, by = "site") %>%
  mutate(site = substring(site, 4)) %>% 
  filter(catchment %in% c("Waimea")) %>%
  ggplot(aes(x = datetime, y = flow, color = site)) +
  geom_line() +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", color = "Site", title = "Waimea") +
  scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(flows$datetime, na.rm = TRUE), by = "6 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(glue("outputs/catchment_flows/Waimea.jpeg"), width = 10, height = 6)

# Plot local Motueka flows over time
plot_motueka_flows <- left_join(flows, sites, by = "site") %>%
  mutate(site = substring(site, 4)) %>% 
  filter(catchment %in% c("Motueka")) %>%
  ggplot(aes(x = datetime, y = flow, color = site)) +
  geom_line() +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", color = "Site", title = "Motueka") +
  scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(flows$datetime, na.rm = TRUE), by = "6 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(glue("outputs/catchment_flows/Motueka.jpeg"), width = 10, height = 6)

# Plot local Riwaka flows over time
plot_motueka_flows <- left_join(flows, sites, by = "site") %>%
  mutate(site = substring(site, 4)) %>% 
  filter(catchment %in% c("Riwaka")) %>%
  ggplot(aes(x = datetime, y = flow, color = site)) +
  geom_line() +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", color = "Site", title = "Riwaka") +
  scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(flows$datetime, na.rm = TRUE), by = "6 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(glue("outputs/catchment_flows/Riwaka.jpeg"), width = 10, height = 6)

# Plot local Golden Bay flows over time
plot_goldenbay_flows <- left_join(flows, sites, by = "site") %>%
  mutate(site = substring(site, 4)) %>% 
  filter(catchment %in% c("Takaka", "Aorere")) %>%
  ggplot(aes(x = datetime, y = flow, color = site)) +
  geom_line() +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", color = "Site", title = "Golden Bay") +
  scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(flows$datetime, na.rm = TRUE), by = "6 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(glue("outputs/catchment_flows/Golden_Bay.jpeg"), width = 10, height = 6)

###################

plot_event_flow_for_site <- function(site, rainfall_site = NA) {
  # rainfall_site, if na then choose the closest rainfall gauge
  print(site)

  flows_site <- flows %>%
    filter(site == !!site) %>%
    left_join(sites, by = "site")

  catchment <- flows_site$catchment[1]

  if (is.na(rainfall_site)) {
    x <- flows_site %>%
      slice(1) %>%
      select(longitude, latitude)
    x_s2 <- s2_lnglat(x$longitude, x$latitude)

    y <- rainfall_sites %>%
      filter(!site %in% unreliable_rainfall_sites) %>% # remove unrealiable rain gauges for event
      left_join(sites, by = "site")
    y_s2 <- s2_lnglat(y$longitude, y$latitude)

    nearest_rainfall_site <- y[s2_closest_feature(x_s2, y_s2), 1]$site
  } else {
    nearest_rainfall_site <- rainfall_site
  }

  # find nearest rainfall site TODO - replace with rainfall gauges in catchment
  rainfall_nearest <- rainfall %>%
    filter(site == !!nearest_rainfall_site)

  thresholds_site <- flow_thresholds %>%
    filter(site == !!site) %>%
    head(1) %>% 
    select_if(~ !any(is.na(.)))

  threshold_limits <- thresholds_site %>%
    select(ends_with("limit")) %>%
    slice(1) %>%
    unlist(use.names = FALSE)
  threshold_labels <- thresholds_site %>%
    select(ends_with("label")) %>%
    slice(1) %>%
    unlist(use.names = FALSE)

  thresholds_all <- tibble(x = as_datetime(default_start_date, tz = "Etc/GMT+12"), limit = threshold_limits, label = threshold_labels)
  highest_record <- thresholds_all %>%
    filter(grepl("Highest", label)) %>%
    mutate(x = x + days(1))
  if (nrow(highest_record) == 0) {
    highest_record <- tibble(x = as_datetime(default_start_date, tz = "Etc/GMT+12"), limit = -999, label = "")
  }
  thresholds_plot <- thresholds_all %>% filter(!grepl("Highest", label)) %>%
    filter(limit > max(flows_site$flow)) %>%  # only plot closest threshold
    head(1)
  if (nrow(thresholds_plot) == 0) {
    thresholds_plot <- thresholds_all %>% filter(!grepl("Highest", label)) %>% 
      arrange(desc(limit)) %>% 
      head(1)
  }

  p1 <- flows_site %>%
    ggplot(aes(x = datetime, y = flow)) +
    geom_line(size = 1.2, color = "red") +
    geom_area(fill = "red", alpha = 0.4) +
    
    geom_hline(yintercept = thresholds_plot$limit, linetype = "dashed", color = "black") +
    geom_text(data = thresholds_plot, aes(x, limit, label = label, hjust = 0, vjust = 1.5), size = 4, color = "black") +
    
    #geom_hline(yintercept = highest_record$limit, linetype = "dashed", color = "magenta") +
    #geom_text(data = highest_record, aes(x, limit, label = label, hjust = 0, vjust = -0.4), size = 2.2) +
    
    theme_bw() +
    labs(x = "Datetime (NZST)", y = "Flow (m3/s)", title = glue("{substring(site, 4)} Flow")) +
    scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(flows$datetime, na.rm = TRUE), by = "12 hours"), date_labels = "%Y%m%d-%H") +
    scale_y_continuous(limits = c(0, max(c(flows_site$flow, thresholds_plot$limit), na.rm = TRUE) * 1.05), expand = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.ticks.y.left = element_line(colour = "red"),
          axis.title.y.left = element_text(colour = "red"),
          axis.line.y.left = element_line(color = "red"),
          axis.text.y.left = element_text(color = "red"))

  total_rainfall <- round(sum(rainfall_nearest$rainfall_total), 0)

  rainfall_annotations <- data.frame(
    xpos = c(min(rainfall_nearest$datetime, na.rm = TRUE) + (max(rainfall_nearest$datetime, na.rm = TRUE) - min(rainfall_nearest$datetime, na.rm = TRUE))/2),
    ypos = c(Inf),
    annotateText = c(glue("Total rainfall: {total_rainfall} mm")),
    hjustvar = c(1.0),
    vjustvar = c(1.6)
  )
  
  coeff <- 10
  
  p2 <- rainfall_nearest %>%
    ggplot(aes(x = datetime, y = rainfall_total)) +
    geom_col(color = "blue", fill = "blue", alpha = 0.4) +
    geom_line(aes(x = datetime, y = cumsum((rainfall_total))/coeff), size = 0.8, color = "magenta", linetype = "twodash", inherit.aes = FALSE) +
    scale_y_continuous(
      name = "Hourly Rainfall (mm)",
      limits = c(0, total_rainfall/coeff * 1.05),
      sec.axis = sec_axis(~.*coeff, name="Cumulative Rainfall (mm)"),
      expand = c(0, NA)
    ) + 
    geom_text(data = rainfall_annotations, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText), size = 4.5, color = "blue", alpha = 0.9) +
    theme_bw() +
    labs(x = "Datetime (NZST)", title = glue("{substring(nearest_rainfall_site, 4)} Rainfall")) +
    scale_x_datetime(breaks = seq(as_datetime(default_start_date, tz = "Etc/GMT+12"), max(rainfall_nearest$datetime, na.rm =TRUE), by = "12 hours"), date_labels = "%Y%m%d-%H") +
    #scale_y_continuous(limits = c(0, max(rainfall_nearest$rainfall_total) * 1.05), expand = c(0, NA)) +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.ticks.y.left = element_line(colour = "blue"),
          axis.title.y.left = element_text(colour = "blue"),
          axis.line.y.left = element_line(color = "blue"),
          axis.text.y.left = element_text(color = "blue"),
          axis.ticks.y.right = element_line(colour = "magenta"),
          axis.title.y.right = element_text(colour = "magenta", angle = 90),
          axis.line.y.right = element_line(color = "magenta"),
          axis.text.y.right = element_text(color = "magenta"))

  print(catchment)
  if (catchment == "Nelson") {
    theme_border <- theme_gray() +
      theme(plot.background = element_rect(fill = NA, colour = "#0074c5", size = 3))
  } else{
    theme_border <- theme_gray() +
      theme(plot.background = element_rect(fill = NA, colour = "#273691", size = 3))
  }

  p <- (p2 / p1) +
    plot_annotation(theme = theme_border)

  if (!file.exists(glue("outputs/summary/{catchment}"))) {
    dir.create(glue("outputs/summary/{catchment}"))
  }

  ggsave(glue("outputs/summary/{catchment}/{site}.png"), width = 6, height = 6)
}

# site <- "HY Maitai at Avon Tce"
# plot_event_flow_for_site(site)

flow_rainfall_mapping <- tibble(data = list(c("HY Brook at Seymour Ave", "HY Nelson at Princes Dr"),
                                            c("HY Maitai at Avon Tce", "HY Nelson at Princes Dr"), 
                                            c("HY Riwaka at Hickmotts", "HY Riwaka South at Moss Bush"),
                                            c("HY Waimea at TDC Nursery", "HY Wairoa at Haycock Rd"))) %>%
  unnest_wider(data) %>%
  `colnames<-`(c("flow_site", "rainfall_site"))

sites_to_plot <- flow_sites$site
#sites_to_plot <- c("HY Maitai at Avon Tce", "HY Maitai at Forks")

# TODO update selected rainfall plots for flow sites
for (site in sites_to_plot) {
  skip_to_next <- FALSE

  rainfall_site <- NA
  if (site %in% flow_rainfall_mapping$flow_site) {
    rainfall_site <- filter(flow_rainfall_mapping, flow_site == !!site)$rainfall_site
  }

  tryCatch(plot_event_flow_for_site(site, rainfall_site), error = function(e) {
    skip_to_next <<- TRUE
  })
}
