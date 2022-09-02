# In development
library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)

get_flow_data <- function(from = "", to = "") {
  get_data_collection(
    collection = "ActiveFlowSites", method = "Average",
    time_interval = NA, from = from, to = to, interval = "15 minutes", alignment = "00:00"
  ) %>%
    rename(flow = value) %>%
    group_by(site) %>%
    slice(-1) %>%
    ungroup()
}

default_start_date <- ymd("2022-09-01")
flows <- get_flow_data(from = format(default_start_date, "%Y%m%d"), to = "Now")

site <- "HY Motueka at Woodmans Bend"

x <- flows %>% 
  filter(site == !!site & yday == 245) %>% 
  group_by(day_hour) %>% 
  summarise(flow = mean(flow, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(day_hour))

p1 <- x %>%
  ggplot(aes(x = datetime, y = flow)) +
  geom_line(size = 1.2, color = "red") +
  geom_area(fill = "red", alpha = 0.4) +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", title = glue("{substring(site, 4)} Flow")) +
  scale_x_datetime(breaks = seq(min(x$datetime, na.rm = TRUE), max(x$datetime, na.rm = TRUE), by = "3 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(min(x$flow, na.rm = TRUE) * 0.95, max(xx$flow, na.rm = TRUE) * 1.05), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 90),
        axis.ticks.y.left = element_line(colour = "red"),
        axis.title.y.left = element_text(colour = "red"),
        axis.line.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"))
p1