library(tidyverse)

source("functions.R")

tdc_logo <- get_png("tdc_logo.png")

from <- "Data start" # Data start"
to <- "20230530" # "Data end"
target_year <- 2023
max_rainfall <- 1000

breaks <- seq(1980, 2025, by = 5)

s <- "HY Waingaro at Hanging Rock"
rainfall <- get_rainfall_daily_data(site = NA, from = from, to = to)

generate_historic_cumulative_plot <- function(s) {
  tryCatch(
    {
      # filter data and prepare for plotting
      rainfall_site <- rainfall %>%
        subset(site == s)

      year_counts <- rainfall_site %>%
        group_by(year) %>%
        summarize(total_days = n())

      filtered_years <- year_counts %>%
        filter(total_days >= 360)

      rainfall_site <- rainfall_site %>%
        filter(year %in% append(filtered_years$year, target_year)) %>%
        arrange(date) %>%
        mutate(
          year_fct = factor(year),
          date_monthday = make_date(year = 2000, month = as.numeric(month), day = day)
        ) %>%
        group_by(date_monthday) %>%
        mutate(
          rainfall_total_mm_average = mean(rainfall_total_mm)
        ) %>%
        ungroup() %>%
        group_by(month, year) %>%
        mutate(rainfall_total_mm_month = sum(rainfall_total_mm)) %>%
        ungroup() %>%
        group_by(month) %>%
        mutate(rainfall_total_mm_median = median(rainfall_total_mm_month)) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(cumulative_rainfall = cumsum(rainfall_total_mm)) %>%
        ungroup()

      # Define the breaks for the 5 year groupings

      rainfall_site$year_group <- cut(rainfall_site$year, breaks = breaks, labels = paste0(breaks[-length(breaks)], "-", breaks[-1]), include.lowest = TRUE)
      # Convert years into 5-year bins with labels
      bins <- cut(rainfall_site$year, breaks = breaks, labels = paste0(breaks[-length(breaks)], "-", breaks[-1]), include.lowest = TRUE)

      rainfall_site_avg <- rainfall_site %>%
        select(date_monthday, rainfall_total_mm_average) %>%
        mutate(rainfall_total_mm_average = round(rainfall_total_mm_average), ) %>%
        unique() %>%
        arrange(date_monthday) %>%
        mutate(
          cumulative_rainfall_average = cumsum(rainfall_total_mm_average)
        )

      rainfall_site_median <- rainfall_site %>%
        select(date_monthday, month, rainfall_total_mm_median) %>%
        mutate(rainfall_total_mm_median = round(rainfall_total_mm_median)) %>%
        unique() %>%
        arrange(month) %>%
        group_by(month) %>%
        select(month, rainfall_total_mm_median) %>%
        unique() %>%
        ungroup() %>%
        mutate(
          cumulative_rainfall_median = cumsum(rainfall_total_mm_median)
        )

      rainfall_site_median <- rainfall_site_avg %>%
        select(date_monthday) %>%
        mutate(month = month(date_monthday, label = TRUE)) %>%
        left_join(rainfall_site_median, by = "month")

      rainfall_target_year <- rainfall_site %>%
        subset(year == target_year)

      max_rainfall <- plyr::round_any(max(rainfall_site$cumulative_rainfall), 100, f = ceiling) # max cumulative rainfall round up to the nearest 100

      p <- ggplot() +
        geom_line(rainfall_site, mapping = aes(x = date_monthday, y = cumulative_rainfall, color = year_group, group = year), alpha = 0.3) +
        geom_line(rainfall_site_avg, mapping = aes(x = date_monthday, y = cumulative_rainfall_average), color = "white", size = 1.5, alpha = 0.3) + # plot average line
        annotate("text", x = as.Date("2000-12-01"), y = filter(rainfall_site_avg, date_monthday == "2000-12-01")$cumulative_rainfall_average, label = "AVG", color = "white", size = 6, fontface = "bold") +
        # geom_line(rainfall_site_median, mapping = aes(x = date_monthday, y = cumulative_rainfall_median), color = "black", linetype = "dashed", size = 1.5, alpha = 0.7) + # plot median line
        # annotate("text", x = as.Date("2000-12-17"), y = filter(rainfall_site_median, date_monthday == "2000-12-17")$cumulative_rainfall_median - 50, label = "MED", size = 6, fontface = "bold") +
        geom_line(subset(rainfall_site, year == target_year), mapping = aes(x = date_monthday, y = cumulative_rainfall), color = "red", size = 2, alpha = 0.7) + # plot target year in red
        annotate("text", x = max(rainfall_target_year$date_monthday), y = max(rainfall_target_year$cumulative_rainfall) + 20, label = glue("{target_year}"), color = "red", size = 5, fontface = "bold", hjust = 0, vjust = 0) +
        geom_text(data = filter(rainfall_site, date_monthday == "2000-12-31"), mapping = aes(x = max(date_monthday) - days(3), y = cumulative_rainfall, label = year), color = "white", hjust = 0, size = 0.8) +
        scale_x_date(
          date_labels = "%b",
          date_breaks = "1 month",
          limits = as.Date(c("2000-01-01", "2000-12-31")),
          expand = c(0, 0)
        ) +
        scale_y_continuous(limits = c(0, max_rainfall + 100), expand = c(0, 0)) +
        scale_colour_brewer(palette = "Set1", name = "Year bin") +
        labs(
          x = "", y = "Rainfall (mm)", title = glue("Yearly Cumulative Rainfall ({min(as.numeric(rainfall_site$year))}-{max(as.numeric(rainfall_site$year))})"),
          subtitle = glue("{substring(s, 4)} Rainfall Data")
        ) +
        theme(
          plot.title = element_text(color = "#273691", hjust = 0, vjust = 1, size = rel(2)),
          plot.subtitle = element_text(color = "#273691", hjust = 0, vjust = 1, size = rel(1.4)),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_rect(fill = "grey10"),
          panel.border = element_rect(fill = NA, color = "#273691", size = 0.5, linetype = "solid"),
          panel.grid.major.x = element_blank(), # element_line(color = "white", size = 0.05),
          panel.grid.major.y = element_blank(), # element_line(color = "white", size = 0.1),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_line(color = "#273691"),
          axis.text = element_text(color = "#273691", size = rel(1.5)),
          axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_text(color = "#273691", size = rel(1.5), face = "bold"),
          legend.text = element_text(color = "#273691", size = rel(0.9)),
          legend.background = element_rect(fill = "grey10"),
          legend.position = "right",
          legend.title = element_text(color = "#273691", size = rel(1.1)),
          legend.key = element_blank()
        )

      c_p <- cowplot::ggdraw(p) +
        cowplot::draw_plot(tdc_logo, x = 0.76, y = 0.9, width = 0.15, height = 0.15) # Add the logo

      ggsave(glue("outputs/cumulative/{substring(s, 4)}_rainfall_cumulative_summary.png"), plot = c_p, width = 15, height = 10, dpi = 300)
    },
    error = function(e) {
      print(s)
    }
  )
}

lapply(unique(rainfall$site), generate_historic_cumulative_plot)
