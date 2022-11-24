library(tdcR)
library(tidyverse)
library(glue)
library(lubridate)

site <- "GW 2166 - Tui Close"
site <- "HY Motueka at Parker St"

measurement <- "Rainfall"

# Note when providing daily results it is best to use the server statistics function.
data <- get_data_site_measurement(site = site, measurement = measurement, method = "Total", from = "DataStart", to = "Now", interval = "1 day") %>% 
  select(date, value) %>% 
  rename(rainfall_mm = value)

data %>% write_csv(glue("outputs/{site}.csv"))

# By collection
sites <- get_sites(latlong = FALSE)

data2 <- get_data_collection(collection = "rainfall",  method = "Total", from = "20100101", to = "Now", interval = "1 day") %>% 
  select(site, date, value) %>% 
  mutate(value = round(value, 1)) %>% 
  rename(rainfall_mm = value) %>% 
  left_join(sites, by = "site") %>% # Cleaning up the data to remove 1 day before start of period
  mutate(year = year(date)) %>% 
  subset(year > 2009) %>% 
  select(-year)

data2 %>% write_csv(glue("outputs/20221115_tasman_rainfall_last10years.csv"))
