library(tidyverse)
library(lubridate)
library(tdcR)

flow_data <- get_data_collection(collection = "ActiveFlowSites", interval = "1 hour", time_interval = "P7D") %>% 
  rename(flow = value)
# modify as needed

write_csv(flow_data, "xx_flow_data.csv")