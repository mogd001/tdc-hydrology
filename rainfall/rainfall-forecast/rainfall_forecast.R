library(tdcR)
library(tidyverse)
library(lubridate)
library(glue)

get_forecast_rainfall <- function(from = "", to = "") {
  get_data_collection(endpoint = "http://envdata.tasman.govt.nz/forecasts.hts?", collection = "ForecastRF", method = "Total", 
                      time_interval = NA, from = from, to = to, interval = "1 hour", alignment = "00:00") %>%
    rename(forecast_rainfall_total_mm = value) %>%
    group_by(site) %>%
    arrange(datetime) %>%
    slice(-1) %>% 
    ungroup() %>%
    mutate(
      interval = hours(1),
      datetime = datetime + interval,# totals with rainfall so add one hour, "rainfall up to hour"
      forecast_rainfall_total_mm = round(forecast_rainfall_total_mm, digits = 2)
    ) %>%
    filter(!is.na(forecast_rainfall_total_mm)) %>% 
    mutate(
      site_name = substring(site, 4)
    ) 
}


from <- format(now(), "%Y%m%d")
to = "Data End"
forecast_rainfall <- get_forecast_rainfall(from, "Data End")

r_site <- "HY Anatoki at Paradise"

coeff <- 10

plot_data <- forecast_rainfall %>%
  subset(site == r_site) 

plot_data %>% 
  ggplot(aes(x = datetime, y = forecast_rainfall_total_mm)) +
  geom_col(color = "blue", fill = "blue", alpha = 0.4) +
  geom_line(aes(x = datetime, y = cumsum((forecast_rainfall_total_mm))/coeff), size = 0.8, color = "magenta", linetype = "twodash", inherit.aes = FALSE) +
  scale_y_continuous(
    name = "Hourly Rainfall (mm)",
    limits = c(0, NA),
    sec.axis = sec_axis(~.*coeff, name="Cumulative Rainfall (mm)"),
    #expand = c(0, 1)
  ) + 
  theme_bw() +
  labs(x = "Datetime (NZST)", title = glue("{r_site} Rainfall")) +
  #scale_x_datetime(breaks = seq(now(), max(forecast_rainfall$datetime, na.rm =TRUE), by = "12 hours"), date_labels = "%Y%m%d-%H") +
  theme(
        axis.ticks.y.left = element_line(colour = "blue"),
        axis.title.y.left = element_text(colour = "blue"),
        axis.line.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"),
        axis.ticks.y.right = element_line(colour = "magenta"),
        axis.title.y.right = element_text(colour = "magenta", angle = 90),
        axis.line.y.right = element_line(color = "magenta"),
        axis.text.y.right = element_text(color = "magenta"))

####### windy
library(httr)
library(jsonlite)
# https://api.windy.com/point-forecast/docs

body <- list(lat=-41.40808, lon= 173.159, model= "gfs", parameters = list("precip", "wind"), levels = list("surface", "800h"), key = "o0Zflj369Lwa4LmZqIGx2eRY5zLJbGDe")

body <- jsonlite::toJSON(body, auto_unbox=TRUE)

res = POST(url = "https://api.windy.com/api/point-forecast/v2",
           body = body,
           verbose(),
           content_type_json()           
)

data = fromJSON(rawToChar(res$content))

data$ts
data$`past3hprecip-surface`

rain <- tibble(ts = data$ts, past3hprecip_mm = data$`past3hprecip-surface`* 1000) %>% 
  mutate(datetime = as_datetime(ts/1000, tz="NZ"))

ggplot(rain, aes(datetime, past3hprecip_mm)) +
  geom_col()


####### metservice
from <- format(now(), "%Y-%m-%dT00:00:00Z")
to <- format(now() +  days(3),  "%Y-%m-%dT00:00:00Z")

library(httr)
library(jsonlite)
#https://forecast-docs.metoceanapi.com/docs/#/endpoint-point-time

body <- list(points = list(list(lat=-40.91107, lon= 172.6177)), variables= list("precipitation.rate"), time = list(from = from, interval = "1h", to = to))
body <- jsonlite::toJSON(body, auto_unbox=TRUE)

res = POST(url = "https://forecast-v2.metoceanapi.com/point/time",
           body = body,
           add_headers("x-api-key" = "HLAVnP7HzDC6od8iurRyna"),
           verbose(),
           content_type_json()         
)

data = fromJSON(rawToChar(res$content))

rain <- tibble(datetime = ymd_hms(data$dimensions$time$data, tz = "NZ"), 
       rainfall_mm = data$variables$precipitation.rate$data)

ggplot(rain, aes(datetime, rainfall_mm)) +
  geom_col()

