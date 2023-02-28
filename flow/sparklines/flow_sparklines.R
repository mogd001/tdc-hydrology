library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)
library(sf)
library(plotly)
library(scales)
library(htmltools)
library(htmlwidgets)

unlink("flow_outputs/*", recursive = TRUE)

catchments <- sf:: st_read("data/catchments.gpkg", layer = "catchments") %>% 
  mutate(catchment = factor(catchment,
                            ordered = TRUE,
                            levels=c("Aorere", "Takaka", "Riwaka", "Motueka", "Marahau", "Moutere", "Waimea", "Nelson", "Buller", "Other")))

st_layers("data/catchments.gpkg")
sites <- get_sites() %>%
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

flow_data <- get_data_collection(collection = "ActiveFlowSites", interval = "1 hour", time_interval = "P7D") %>% 
  rename(flow = value) %>% 
  mutate(
    datetime = with_tz(datetime, "NZ"),
    date = as.numeric(format(as.Date(datetime, t= "NZ"), "%Y%m%d")),
    flow = round(flow, 2)) %>% 
  left_join(site_catchment, by = "site")

max_datetime <- max(flow_data$datetime)
min_datetime <- max_datetime - days(7)

flow_data_p7d <- flow_data %>% 
  filter(datetime >= min_datetime)

# Create labelleler to modify facet wraps
catchment_names <- lapply(catchments$catchment, function(x) "")
names(catchment_names) <- catchments$catchment

site_names <- unique(flow_data$site)
names(site_names) <- site_names

plot_labeller <- function(variable, value) {
  if (variable == "catchment") {
    return(as.character(catchment_names[value]))
  } else {
    return(as.character(site_names[value]))
  }
}

flow_data_p7d_highlight <- flow_data_p7d %>% 
  rename(dt = datetime, f = flow)

# ggplot
p <- ggplot() +
  geom_line(flow_data_p7d, mapping = aes(x = datetime, y = flow, color = catchment)) +
  facet_wrap(catchment~site, scales = "free_y", labeller = plot_labeller) +
  labs(y = NULL, x = NULL, color = "Catchment", title = glue("Flow P7D (m3/s) {min_datetime} - {max_datetime}  [NZDT]")) +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 1),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))

ggsave(glue("flow_outputs/{as.Date(max_datetime, tz = 'NZ')}_rainfall_sparklines.png"), p, dpi = 300, height = 10, width = 16)

# ggplotly
p <- ggplot() +
  geom_line(flow_data_p7d_highlight, mapping = aes(x = dt, y = f, frame = date), size = 2, color = "red", alpha = 0.2) + 
  geom_line(flow_data_p7d, mapping = aes(x = datetime, y = flow, color = catchment)) +
  facet_wrap(catchment~site, scales = "free_y", labeller = plot_labeller) +
  labs(y = NULL, x = NULL, color = "Catchment", title = glue("Flow P7D (m3/s) {min_datetime} - {max_datetime}  [NZDT]")) +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 1),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))

plotly_p <- ggplotly(p, tooltip = c("datetime",  "flow"), height = 800, width = 1400) %>%
  layout(hovermode = "x") %>%  
  config(labeller = as_labeller(plot_labeller)) %>% 
  layout(hovermode = 'x unified') %>% 
  animation_opts(easing = "elastic") %>% 
  animation_button(visible = FALSE)

save_html(plotly_p, file = glue("flow_outputs/{as.Date(max_datetime, tz = 'NZ')}_flow_sparklines.html"))
