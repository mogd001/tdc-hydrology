library(tidyverse)
library(ggplot2)
library(tdcR)
library(lubridate)
library(viridis)
library(glue)
library(plotly)
library(gridExtra)
library(scales)

library(zoo)
library(Hilltop)
library(broom)
library(lubridate)

max_rainfall <- 1000
site <- "HY Temp Site 1"

data <- HilltopData("M:/Datafiles/Hydro-Working.DSN") 
rainfall <- GetData(data, glue("{site}"), "Rainfall", "", "", method = "Total", interval = "1 months") %>% 
  tidy()
disconnect(data)

# Method 1
rainfall <- rainfall %>% 
  transmute(datetime = index - months(1), rainfall_total_mm = value) %>%
  mutate(month = month(datetime),
         year = year(datetime)) %>% 
  filter(year >= 1902) %>% 
  drop_na()

# Method 2
rainfall <- tdcR::get_data_site_measurement(site = "HY Temp Site 1", measurement = "Rainfall", method = "Total", interval = "1 month", from = "Data start", to = "Data end") %>%
  rename(rainfall_total_mm = value) %>%
  mutate(site = site,
         month = month(datetime)) %>%
  select(site, year, month, rainfall_total_mm)

  
generate_historic_report <- function(s) {
  rainfall_site <- rainfall %>%
    subset(site == s) %>%
    mutate(rainfall_total_label = ifelse(rainfall_total_mm > max_rainfall, ">1000", as.character(round(rainfall_total_mm, 0))),
           rainfall_total_mm = ifelse(rainfall_total_mm > max_rainfall, 1000, round(rainfall_total_mm, 0)))
  
  p <- ggplot(rainfall_site, aes(year, month, fill = rainfall_total_mm)) +
    geom_tile(color = "gray20", size = 1.5, stat = "sum") +
    geom_text(aes(label = rainfall_total_label), size = 1, nudge_x = 0, nudge_y = 0, color = "white") +
    scale_fill_viridis_c(option = "turbo", limits = c(0, 1000)) +
    scale_x_continuous(limits = c(1878, NA)) +
    scale_y_continuous(breaks = 1:12, labels = month.abb[1:12]) +
    labs(x = "", y = "", title = "Total Rainfall By Month and Year", subtitle = glue("{substring(s, 4)}"), fill = "Rainfall (mm)") +
    theme(
      plot.title = element_text(color = "white", hjust = 0, vjust = 1, size = rel(2)),
      plot.subtitle = element_text(color = "white", hjust = 0, vjust = 1, size = rel(1.4)),
      plot.background = element_rect(fill = "gray20"),
      panel.background = element_rect(fill = "gray20"),
      panel.border = element_rect(fill = NA, color = "gray20", size = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(color = "white", size = rel(1.5)),
      axis.text.y = element_text(hjust = 1),
      legend.text = element_text(color = "white", size = rel(0.9)),
      legend.background = element_rect(fill = "gray20"),
      legend.position = "bottom",
      legend.title = element_text(color = "white", size = rel(1.1))
    ) +
    guides(fill = guide_colourbar(title.position = "bottom"))
  
  ggsave(glue("{substring(s, 4)}_rainfall_summary.png"), plot = p, width = 20, height = 10, dpi = 300)
}

generate_historic_report(site)
