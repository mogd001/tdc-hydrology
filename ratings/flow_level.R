library(zoo)
library(tidyverse)
library(lubridate)
library(Hilltop)

??Hilltop

hydro_working_dsn <- r"{\\tsrvfiles\hydrology\Datafiles\Hydro-Working.dsn}"
site <- "HY Motueka at Gorge"
start_date = "20220101"
end_date = format(Sys.Date(), format = "%Y%m%d")

# Setup base datetime tibble
df <- tibble(
  datetime = seq(
    ymd_hms(paste(start_date, "00:00:00", sep=" ")), ymd_hms(paste(end_date, "00:00:00", sep=" ")), by = "5 mins")
) %>%
  mutate(
    hour = hour(datetime),
    weekday = wday(datetime, week_start = 1),
    date = as.Date(datetime)
  )

# Get stage and flow data from hilltop
conn <- HilltopData(hydro_working_dsn)

# Stage
stage <- GetData(conn, site, "Stage", start_date, end_date) %>%
  as.data.frame() %>%
  rename("stage" = ".") %>%
  cbind(datetime = rownames(stage)) %>% 
  mutate(
    datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"),
  ) %>%
  as_tibble()

# Flow
flow <- GetData(conn, site, "Flow", start_date, end_date) %>%
  as.data.frame() %>%
  rename("flow" = ".") %>%
  cbind(datetime = rownames(flow)) %>% 
  mutate(
    datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"),
  ) %>%
  as_tibble()

disconnect(conn)

# Tidy
df <- df %>% 
  left_join(stage[, c("datetime", "stage")], by = "datetime") %>%
  left_join(flow[, c("datetime", "flow")], by = "datetime")

# Plot
ggplot(data = df, mapping = aes(x = stage, y = flow)) +
  geom_point() + 
  theme_classic() + 
  labs(subtitle="Rating curve", 
       y="Flow", 
       x="Stage", 
       title="Flow vs Stage", 
       caption="Source: Hilltop")

  