library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)


rainfall <- get_data_collection(collection = "AllRainfall",
                            method = "Total", interval = "1 hour", from = "20230331", to = "Now") %>% 
  mutate(datetime = with_tz(datetime, "NZ")) %>% 
  rename(rainfall_total_mm = value) 

site <- "HY Anatoki at Paradise"

r <- rainfall %>% 
  filter(site == !!site)

coeff <- 20

p <- r %>%
  mutate(datetime = with_tz(datetime, "NZ")) %>% 
  ggplot(aes(x = datetime - minutes(30), y = rainfall_total_mm)) +
  geom_col(color = "blue", fill = "blue", alpha = 0.4) +
  geom_line(aes(x = datetime, y = cumsum((rainfall_total_mm))/coeff), size = 0.8, color = "magenta", linetype = "twodash", inherit.aes = FALSE) +
  scale_y_continuous(
    name = "Hourly Rainfall (mm)",
    limits = c(0,  15),
    #limits = c(0,  max(r$rainfall_total_mm) * 1.05),
    sec.axis = sec_axis(~.*coeff, name="Cumulative Rainfall (mm)"),
  ) + 
  theme_bw() +
  labs(x = "Datetime (NZDT)", title = glue("{site} Rainfall")) +
  scale_x_datetime(breaks = seq(min(r$datetime, na.rm =TRUE), max(r$datetime, na.rm =TRUE), by = "1 hours"), date_labels = "%Y%m%d-%H") +
  theme(axis.text.x = element_text(angle = 45 , vjust = 1, hjust=1),
        axis.ticks.y.left = element_line(colour = "blue"),
        axis.title.y.left = element_text(colour = "blue"),
        axis.line.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"),
        axis.ticks.y.right = element_line(colour = "magenta"),
        axis.title.y.right = element_text(colour = "magenta", angle = 90),
        axis.line.y.right = element_line(color = "magenta"),
        axis.text.y.right = element_text(color = "magenta"))


# Hawkes Bay explore
#https://www.hbrc.govt.nz/environment/rainfall/
sites <- get_sites(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?")

site <- "Hangaroa River at Doneraille Park" # Waipukurau Climate #"Hangaroa River at Doneraille Park"
r <- get_data_site_measurement(endpoint = "https://data.hbrc.govt.nz/Envirodata/EMAR.hts?", site = site, measurement = "Rainfall", method = "Total", interval = "1 hour", time_interval = "P7D") %>% 
  rename(rainfall_total_mm = value)


# Daily totals
rainfall <- get_data_collection(collection = "AllRainfall",
                                method = "Total", interval = "1 day", from = "20230410", to = "Now") %>% 
  mutate(datetime = with_tz(datetime, "NZ"),
         date = date(datetime) - days(1)) %>% 
  
  rename(rainfall_total_mm = value) 

site <- "HY Richmond Weather at TDC Roof"

r <- rainfall %>% 
  filter(site == !!site)