library(tidyverse)
library(hillr)
library(XML)
library(rvest)
library(lubridate)
library(plotly)
library(htmltools)

################################################################################
# 
# Title: Rainfall Map Summary
# Author: Matt Ogden
# Description: This script joins the rainfall summary data and creates a map output, 
# showing the total rainfall at the various rainfall sites.
#
################################################################################

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoibWF0dG9nZGVuIiwiYSI6ImNsNDNjY21jNDAxZmYzb3A4NWE2Y2M2cXEifQ.4vG7Zx9grk1z7R9LsDPeEA')
datatables_fp <- r"{\\tsrvfiles\hydrology\Datafiles\Data Tables}"

# Load RainHour
rain_hour <- "RainHour.htm"

latest_hr <- floor_date(now(), "1 hour")
x = c()
for (i in 0:23) {
  x <- append(x, format(latest_hr - hours(i), format = "%H:%M"))  
}
header = append(c("sitename", "total"), rev(x))
rain_hr_df <- read_html(paste0(datatables_fp, "\\", rain_hour)) %>% 
  html_table(fill = TRUE, header = FALSE) %>%
  first()
names(rain_hr_df) <- header

# site_measurements.rds produced in Site-Analysis, contains all sites and corresponding measurements.
sites_measurements <- readRDS(file = "data/sites_measurements.rds")

rainfall_sites <- sites_measurements %>% 
  filter(measurementname == "Rainfall") %>%
  mutate(sitename = substring(site, 4)) %>%
  left_join(rain_hr_df, by = "sitename") %>%
  drop_na(total)

# Plot 
styles <- schema()$layout$layoutAttributes$mapbox$style$values
style_buttons <- lapply(styles, function(s) {
  list(
    label = s, 
    method = "relayout", 
    args = list("mapbox.style", s)
  )
})

p <- layout(
  plot_mapbox(rainfall_sites) %>%
    add_markers(
      x = ~long, 
      y = ~lat,
      size = ~total,
      color = ~measurementname,
      colors = c("blue"),
      text = ~paste0(site, "\n Total rainfall last 24 hours: ", total, " mm")
    ),
    mapbox = list(style = "basic",
                center = list(lat = ~median(lat), lon = ~median(long)),
                zoom = 7.8)
  )
save_html(p, file = "outputs/rainfall_summary.html")
