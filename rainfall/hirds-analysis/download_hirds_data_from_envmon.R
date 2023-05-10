library(tdcR)

sites <- get_collections() %>%
  filter(collection == "Rainfall" & !site %in% c("HY Karamea at Garibaldi", "HY Mokihinui at Stoney Ck"
                                                  )) %>%
  arrange(site) %>% 
  pull(site) 
  

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

get_hirds_table <- function(site) {
  print(site)
  site_information <- get_site_information_envmon(site)
  hirds_table <- tabulate_hirds_data_string(site_information$hirds_data) %>% 
    mutate(ari = fct_rev(factor(ari))) %>%
    pivot_longer(!ari, names_to = "duration_text", values_to = "val") %>%
    left_join(duraction_text_to_val, by = "duration_text") %>%
    filter(ari %in% c(1.58, 2, 5, 10, 20, 50, 100))
}


out <- lapply(sites, get_hirds_table)
names(out) <- sites

saveRDS(out, "20230510_tdc_site_hird_data.rds")
