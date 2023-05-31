library(tidyverse)
library(ggplot2)
library(tdcR)
library(lubridate)
library(viridis)
library(glue)
library(plotly)
library(gridExtra)
library(scales)

source("functions.R")

tdc_logo <- get_png("tdc_logo.png")

from <- "Data start"
to <- "Data end"
max_rainfall <- 1000

rainfall <- get_rainfall_monthly_data(site = NA, from = from, to = to)

unique(rainfall$site)
site <- "HY Waingaro at Hanging Rock"

generate_historic_report <- function(s) {
  tryCatch(
    {
      rainfall_site <- rainfall %>%
        subset(site == s) %>%
        mutate(
          rainfall_total_label = ifelse(rainfall_total_mm > max_rainfall, ">1000", as.character(round(rainfall_total_mm, 0))),
          rainfall_total_mm = ifelse(rainfall_total_mm > max_rainfall, 1000, round(rainfall_total_mm, 0))
        )

      p <- ggplot(rainfall_site, aes(month, year, fill = rainfall_total_mm)) +
        geom_tile(color = "gray20", size = 1, stat = "sum") +
        geom_text(aes(label = rainfall_total_label), size = 2, nudge_x = 0, nudge_y = 0, color = "black") +
        scale_fill_viridis_c(option = "turbo", limits = c(0, 1000)) +
        scale_y_continuous(limits = c(1980, NA)) +
        scale_y_reverse() +
        # scale_y_continuous(breaks = 1:12, labels = month.abb[1:12]) +
        labs(x = "", y = "", title = "Total Rainfall By Month and Year", subtitle = glue("{substring(s, 4)} Rainfall Data"), fill = "Rainfall (mm)") +
        theme(
          plot.title = element_text(color = "#273691", hjust = 0, vjust = 1, size = rel(2)),
          plot.subtitle = element_text(color = "#273691", hjust = 0, vjust = 1, size = rel(1.4)),
          plot.background = element_rect(fill = "gray10"),
          panel.background = element_rect(fill = "gray10"),
          panel.border = element_rect(fill = NA, color = "gray10", size = 0.5, linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(color = "#273691", size = rel(1.5)),
          axis.text.y = element_text(hjust = 1),
          legend.text = element_text(color = "#273691", size = rel(0.9)),
          legend.background = element_rect(fill = "gray10"),
          legend.position = "right",
          legend.title = element_text(color = "#273691", size = rel(1.1))
        ) +
        guides(fill = guide_colourbar(title.position = "bottom"))

      c_p <- cowplot::ggdraw(p) +
        cowplot::draw_plot(tdc_logo, x = 0.76, y = 0.9, width = 0.15, height = 0.15) # Add the logo

      ggsave(glue("outputs/grid/{substring(s, 4)}_rainfall_summary.png"), plot = c_p, width = 10, height = 15, dpi = 300)
    },
    error = function(e) {
      print(s)
    }
  )
}

generate_historic_report(site)

lapply(unique(rainfall$site), generate_historic_report)
