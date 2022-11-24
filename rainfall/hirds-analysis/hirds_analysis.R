library(tdcR) # hilltop server commands
library(tidyverse)
library(lubridate)
require(scales)
library(plotly)
library(htmlwidgets)
library(glue)
library(knitr)
library(kableExtra)

source("functions.R")

site <- "HY Brook at Third House"
from <- "20220816"
to <- "20220822"

site_information <- tdcR::get_site_information_envmon(site)
hirds_table <- tdcR::tabulate_hirds_data_string(site_information$hirds_data)

duraction_text_to_val <- tribble(
  ~duration_text, ~duration, ~duration_interval, ~duration_label,
  "dur_10min", 1 / 6, "10 minutes", "10 min",
  "dur_20min", 2 / 6, "20 minutes", "20 min",
  "dur_30min", 3 / 6, "30 minutes", "30 min",
  "dur_1hour", 1, "1 hour", "1 hour",
  "dur_2hour", 2, "2 hours", "2 hours",
  "dur_6hour", 6, "6 hours", "6 hours",
  "dur_12hour", 12, "12 hours", "12 hours",
  "dur_1day", 24, "24 hours", "1 day",
  "dur_2day", 48, "48 hours", "2 days",
  "dur_3day", 72, "72 hours", "3 days",
  "dur_4day", 96, "96 hours", "4 days",
  "dur_5day", 120, "120 hours", "5 days"
)

nz_record <- tribble(
  ~duration, ~value,
  0.1, 20,
  1, 100,
  10, 500,
  40, 1000,
  100, 1500
)

hirds <- hirds_table %>%
  mutate(ari = fct_rev(factor(ari))) %>%
  pivot_longer(!ari, names_to = "duration_text", values_to = "val") %>%
  left_join(duraction_text_to_val, by = "duration_text") %>%
  filter(ari %in% c(1.58, 2, 5, 10, 20, 50, 100))

intervals <- duraction_text_to_val$duration_interval

observed <- do.call(rbind, lapply(intervals, get_max_rainfall, site = site, from = from, to = to)) %>%
  left_join(duraction_text_to_val, by = "duration_interval") %>%
  mutate(
    ari_for_duration = unlist(map2(duration_label, value, determine_ari_duration, hirds)),
    ari_for_duration = if_else(ari_for_duration == -999, "<1.58", 
                               if_else(ari_for_duration == 999, ">100+", as.character(round(ari_for_duration, 1))))
  )

dplyr::select(observed, c(duration_label, rainfall_mm = value, ari_for_duration)) %>% 
                mutate(rainfall_mm = round(rainfall_mm, 1)) %>% 
                knitr::kable("html", col.names = c("Duration",  "Rainfall (mm)", "ARI")) %>% 
                kable_styling("striped") %>%
                save_kable("temp_table.html")

# Plot 1
arrows <- 
  tibble(
    x1 = c(6),
    x2 = c(24),
    y1 = c(1000), 
    y2 = c(800)
  )

p1 <- ggplot() +
  geom_line(hirds, mapping = aes(duration, val, color = ari), linetype = "dashed") +
  geom_line(observed, mapping = aes(duration, value), color = "magenta", size = 1) +
  geom_line(nz_record, mapping = aes(duration, value), color = "red", size = 1) +
  
  
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 0.5,
    arrow = arrow(length = unit(0.08, "inch")),
    color = "red", curvature = -0.1) +
  geom_label(aes(x = 6, y = 1000, label = "NZ Record"), color = "red", fill = "white") +
  
  geom_point(observed, mapping = aes(duration, value), color = "magenta", size = 2.5) +
  scale_x_continuous(trans = "log10", breaks = duraction_text_to_val$duration, labels = duraction_text_to_val$duration_label) +
  scale_y_log_eng() +
  scale_color_viridis_d() +
  labs(
    x = "Duration", y = "Rainfall (mm)", color = "ARI (years)", linetype = NULL,
    title = glue(
      {
        site
      },
      " ",
      {
        from
      },
      "-",
      {
        to
      },
      " Rainfall"
    )
  ) +
  annotation_logticks(sides = "l") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.minor.x = element_blank()
  )

p1
ggplotly(p1)
htmlwidgets::saveWidget(ggplotly(p1), "hirds_output_example.html")

# Plot 2
p2 <- ggplot() +
  geom_line(hirds, mapping = aes(duration, val, color = ari), linetype = "dashed") +
  geom_line(observed, mapping = aes(duration, value), color = "magenta", size = 1) +
  geom_point(observed, mapping = aes(duration, value), color = "magenta", size = 2.5) +
  geom_line(nz_record, mapping = aes(duration, value, linetype = "NZ Record"), color = "red", size = 1) +
  scale_x_log_eng(limits = c(0.1, 100)) +
  scale_y_log_eng(limits = c(10, 1000)) +
  scale_color_viridis_d() +
  labs(x = "Duration", y = "Rainfall (mm)", color = "ARI (years)", linetype = NULL) +
  annotation_logticks() +
  theme_bw()
