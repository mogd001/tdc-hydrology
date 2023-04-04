library(tdcR)
library(tidyverse)
library(glue)
library(lubridate)

site <- "GW 2166 - Tui Close"
site <- "HY Motueka at Parker St"

measurement <- "Rainfall"

# Note when providing daily results it is best to use the server statistics function.
data <- get_data_site_measurement(site = site, measurement = measurement, method = "Total", from = "DataStart", to = "Now", interval = "1 day") %>% 
  mutate(date = as.Date(datetime, tz = "NZ") - days(1)) %>% 
  select(date, value) %>% 
  rename(rainfall_mm = value)

data %>% write_csv(glue("outputs/{site}.csv"))

# By collection
sites <- get_sites(latlong = FALSE)

data2 <- get_data_collection(collection = "rainfall",  method = "Total", from = "20100101", to = "Now", interval = "1 day") %>% 
  mutate(date = as.Date(datetime, tz = "NZ") - days(1)) %>% 
  select(site, date, value) %>% 
  mutate(value = round(value, 1)) %>% 
  rename(rainfall_mm = value) %>% 
  left_join(sites, by = "site") %>% 
  mutate(year = year(date)) %>% 
  subset(year > 2009) %>% 
  select(-year)

data2 %>% write_csv(glue("outputs/20221115_tasman_rainfall_last10years.csv"))


#### 20230123 Export for Paul Baynham
site1 <- "HY Wai-iti at Belgrove"
site2 <- "HY Richmond Weather at TDC Roof"

site1_data <- get_data_site_measurement(site = site1, measurement = measurement, method = "Total", time_interval = "P7Y", interval = "1 day") %>% 
  mutate(site = site1)
site2_data <- get_data_site_measurement(site = site2, measurement = measurement, method = "Total", time_interval = "P7Y", interval = "1 day") %>% 
  mutate(site = site2)

data <- site1_data %>% 
  rbind(site2_data) %>% 
  mutate(date = date(datetime) - days(1)) %>% 
  select(site, date, value) %>% 
  rename(rainfall_mm = value) %>% 
  filter(date >= ymd("20170101"))

p <- ggplot(data %>% group_by(site) %>% mutate(cumulative_rainfall = cumsum(rainfall_mm))) +
  geom_line(mapping = aes(x = date, y = cumulative_rainfall, color = site))

ggsave("outputs/20230123_daily_rainfall_for_airquality.png", plot = p)

data %>% write_csv(glue("outputs/20230123_daily_rainfall_for_airquality.csv"))


#### 20230327 Export for Cole Brown

sites <- get_sites(collection = "AllRainfall")

site1 <- "HY Nelson at Princes Dr"
site1_data <- get_data_site_measurement(site = site1, measurement = "Rainfall", method = "Total", from = "Data start", to = "Data end", interval = "15 minute") %>% 
  mutate(site = site1) %>% 
  rename(rainfall_mm = value)

site1_out_data <- site1_data %>% 
  mutate(datetime = with_tz(datetime, tz ="NZ"),
         datetime = force_tz(datetime, tz ="UTC"))


site1_out_data %>% write_csv(glue("outputs/20230327_NelsonatPrincesDr.csv"))
