library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)

flows <- get_data_collection("http://envdata.tasman.govt.nz/data.hts?", collection = "ActiveFlowSites",
                            from = "20230220", to = "Now") %>% # time_interval = "P7D"
  rename(flow = value) %>%
  ungroup()

unique(flows$site)
site <- "HY Wairoa at Irvines Bridge"

f <- flows %>% 
  filter(site == !!site) %>%
  mutate(day_hour = format(datetime, "%Y%m%d%H"),
         hour = hour(datetime)) %>% 
  #group_by(day_hour) %>% 
  #summarise(flow = mean(flow, na.rm = TRUE)) %>% 
  #ungroup() %>% 
  #arrange(desc(day_hour)) %>% 
  mutate(day_hour = ymd_h(day_hour, tz = "Etc/GMT-12"),
         day_night = if_else(hour >= 7 & hour < 20, "day", "night"),
         day_night_bin = if_else(day_night == "day", 1, 0)
         )
f$group <- cumsum(c(0, as.numeric(diff(f$day_night_bin))!=0))


p <- ggplot() +
  geom_point(data = f, mapping = aes(x = day_hour, y = flow), color = "red", size = 1.2, alpha = 0.4) +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", title = glue("{substring(site, 4)} Flow")) +
  scale_x_datetime(breaks = seq(min(f$day_hour, na.rm = TRUE), max(f$day_hour, na.rm = TRUE), by = "12 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(min(f$flow, na.rm = TRUE) * 0.95, max(f$flow, na.rm = TRUE) * 1.05), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45 , vjust = 1, hjust=1),
        axis.ticks.y.left = element_line(colour = "red"),
        axis.title.y.left = element_text(colour = "red"),
        axis.line.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"))

p


# day night exploration

p <- ggplot() +
  geom_point(data = f, mapping = aes(x = day_hour, y = flow, color = day_night), size = 1.2, alpha = 0.4) +
  geom_smooth(data = f, mapping = aes(x = day_hour, y = flow, color = day_night, group = group), method = "lm", se = FALSE) +
  geom_smooth(data = f, mapping = aes(x = day_hour, y = flow), se = FALSE) + 
  #geom_area(fill = "red", alpha = 0.4) +
  theme_bw() +
  labs(x = "Datetime (NZST)", y = "Flow (m3/s)", title = glue("{substring(site, 4)} Flow")) +
  scale_x_datetime(breaks = seq(min(f$day_hour, na.rm = TRUE), max(f$day_hour, na.rm = TRUE), by = "12 hours"), date_labels = "%Y%m%d-%H") +
  scale_y_continuous(limits = c(min(f$flow, na.rm = TRUE) * 0.95, max(f$flow, na.rm = TRUE) * 1.05), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45 , vjust = 1, hjust=1),
        axis.ticks.y.left = element_line(colour = "red"),
        axis.title.y.left = element_text(colour = "red"),
        axis.line.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"))
p
