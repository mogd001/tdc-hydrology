library(tdcR)
library(tidyverse)
library(glue)
library(lubridate)

site <- "GW 2166 - Tui Close"
site <- "HY Motueka at Parker St"

measurement <- "Rainfall"

# Note when providing daily results it is best to use the server statistics function.
data <- get_data_site_measurement(site = site, measurement = measurement, method = "Total" , from = "DataStart", to = "Now", interval = "1 day") %>% 
  select(date, value) %>% 
  rename(rainfall_mm = value)

data %>% write_csv(glue("outputs/{site}.csv"))
