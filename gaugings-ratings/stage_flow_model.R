library(remotes)
Sys.unsetenv("GITHUB_PAT")
remotes::install_github("mogd001/tdcR")

library(tidyverse)
library(lubridate)
library(tdcR)
library(glue)

flow_sites <- get_sites(collection = "ActiveFlowSites", synonyms = TRUE)

site <- flow_sites$site[1]
site_name <- flow_sites$second_synonym[1]

ratings <- get_ratings(site = site)

get_data_for_site <- function(site) {
  
  gaugings <- get_gaugings(site = site)
  
  flow <- get_data_site_measurement(site = site, measurement = "Flow", method = "Average", interval = "1 hour", from = "20200101", to = "Data end")  %>% # replace from = ... -> from = "Data start" to fetch all data
    rename(flow_mp3s = value)
  
  list(gaugings, flow)
}


s <- flow_sites$site[1:2]
result <- lapply(s, get_data_for_site)



as.data.frame(do.call(rbind, result))


do.call(rbind, results)

saveRDS(s, "flow_gauging_data.rds")



min_datetime <- min(flow$datetime)

# filter ratings and gaugings 
ratings <- ratings %>% 
  filter(effective_time >= min_datetime)
gaugings <- gaugings %>% 
  filter(datetime >= min_datetime)


# join with flow to get reported flow
gaugings <- gaugings %>% 
  mutate(hr = round_date(datetime, unit = "hour")) %>% 
  left_join(flow, by = c("hr" = "datetime")) %>% 
  rename(
    flow_observed = flow,
    flow_reported = flow_mp3s
  )

max_flow <- max(flow$flow_mp3s, na.rm = TRUE)

g1 <- ggplot() +
  geom_line(data = flow, mapping = aes(x = datetime, y = flow_mp3s)) +
  geom_point(data = gaugings, aes(datetime, max_flow * 0.9), shape = 8, size = 4, color = "red") +
  geom_vline(
    xintercept = ratings$start_time,
    alpha = 0.5,
    lty = 2,
    color = "red"
  ) + 
  scale_x_datetime(breaks = seq(min(flow$datetime), max(flow$datetime), by = "1 years"),
                   date_labels = "%Y") +
  ggtitle(site_name,
          subtitle = "Stars represent gaugings.") +
  labs(x = "", y = "Flow (m3/s)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )) + 
  theme_bw()
g1

g2 <- ggplot() + 
  geom_point(data = gaugings, aes(stage, flow_observed))
g2

####### YOUR MISSION SHOULD YOU CHOOSE TO ACCEPT IT
# Develop a model to predict flow as a function of stage (water level) for all sites, "global model".
# flow_sites is all the flow sites, the visualisations above are just for one site
# gaugings are the measurements at points in time :
#     measurements of stage, flow, area, velocity, max_depth
# ratings are the current system used to model flow as a function of stage. A rating is "modified" after an event when the river morphology.
# You can ignore ratings for now and simply focus on gaugings.



