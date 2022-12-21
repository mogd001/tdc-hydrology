library(tidyverse)
library(ggplot2)
library(tdcR)
library(lubridate)
library(viridis)
library(glue)
library(plotly)
library(gridExtra)
library(scales)

get_rainfall_monthly_data <- function(endpoint = endpoint, from = "", to = "") {
  tdcR::get_data_collection(
    collection = "Rainfall", method = "Total", interval = "1 months",
    from = from, to = to
  ) %>%
    rename(rainfall_total_mm = value) %>%
    mutate(month = month(datetime)) %>%
    select(site, year, month, rainfall_total_mm)
}

from <- "Data start"
to <- "Data end"
max_rainfall <- 1000

rainfall <- get_rainfall_monthly_data(from = from, to = to)

unique(rainfall$site)
site <- "HY Brook at Third House"

generate_historic_report <- function(s) {
  rainfall_site <- rainfall %>%
    subset(site == s) %>%
    mutate(rainfall_total_label = ifelse(rainfall_total_mm > max_rainfall, ">1000", as.character(round(rainfall_total_mm, 0))),
           rainfall_total_mm = ifelse(rainfall_total_mm > max_rainfall, 1000, round(rainfall_total_mm, 0)))

  p <- ggplot(rainfall_site, aes(year, month, fill = rainfall_total_mm)) +
    geom_tile(color = "gray20", size = 1.5, stat = "sum") +
    geom_text(aes(label = rainfall_total_label), size = 2, nudge_x = 0, nudge_y = 0, color = "black") +
    scale_fill_viridis_c(option = "turbo", limits = c(0, 1000)) +
    scale_x_continuous(limits = c(1980, NA)) +
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

  ggsave(glue("outputs/{substring(s, 4)}_rainfall_summary.png"), plot = p, width = 20, height = 10, dpi = 300)
}

generate_historic_report(site)

lapply(unique(rainfall$site), generate_historic_report)
