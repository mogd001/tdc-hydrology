library(tdcR)
library(tidyverse)

sites <- get_sites(latlong = FALSE, synonyms = TRUE) %>% 
  mutate(site_name = substring(site, 4))
  
# OPTION A for getting rainfall sites
collections <- get_collections()

rainfall_sites <- collections %>% 
  filter(collection == "Rainfall") %>% 
  left_join(sites, by = c("site" = "site"))

# OPTION B for getting rainfall sites
rainfall_sites <- get_sites(collection = "Rainfall", synonyms = TRUE) %>% 
  mutate(site_name = substring(site, 4))

nrow(rainfall_sites)

flow_sites <- collections %>% 
  filter(collection == "ActiveFlowSites") %>% 
  left_join(sites, by = c("site" = "site"))

nrow(flow_sites)