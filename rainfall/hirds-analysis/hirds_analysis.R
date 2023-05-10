library(tdcR)
library(tidyverse)
library(lubridate)
require(scales)
library(plotly)
library(htmlwidgets)
library(glue)
library(knitr)
library(kableExtra)

source("functions.R")

site <- "HY Wai-iti at Belgrove" # site <- "HY Anatoki at Paradise"
from <- format(now() - days(5), "%Y%m%d")
to <- "Now"

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
arrow_nz <- tibble(x1 = c(12), x2 = c(23.5), y1 = c(2000), y2 = c(820))
arrow_nt <- tibble(x1 = c(1/2), x2 = c(5/6), y1 = c(400), y2 = c(95))

p1 <- ggplot() +
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
  
  geom_line(observed, mapping = aes(duration, value), color = "magenta", size = 1) +
  geom_point(observed, mapping = aes(duration, value), color = "magenta", size = 2.5) +
  
  scale_x_continuous(trans = "log10", breaks = duraction_text_to_val$duration, labels = duraction_text_to_val$duration_label) +
  scale_y_log_eng(limits = c(7, 2900)) +
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
    plot.title = element_text(colour = "magenta"),
    panel.grid.minor.x = element_blank()
  )

p1
ggsave("outputs/hirds_output_example.png", plot = p1, dpi = 300, height = 8, width = 12)

ggplotly(p1)
htmlwidgets::saveWidget(ggplotly(p1), "outputs/hirds_output_example.html")

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