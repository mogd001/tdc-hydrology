library(tidyverse)
library(lubridate)
library(glue)
library(tdcR)
library(sf)
library(plotly)

library(gstat)
library(raster)
library(car)
library(classInt)
library(RStoolbox)
library(spatstat)
library(dismo)
library(fields)
library(gridExtra)
library(Hmisc)

########### INPUT ########### 
month_year <- ym("2022-07") # define the month to summarise rainfall for
########### #################


get_site_data <- function(endpoint = endpoint) {
  get_sites(endpoint = endpoint) %>%
    mutate(
      longitude_ = longitude,
      latitude_ = latitude
    ) %>%
    st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
    st_transform(crs = 2193) %>% 
    mutate(
      easting = st_coordinates(.)[, "X"],
      northing = st_coordinates(.)[, "Y"]
    )
} 


get_rainfall_monthly_data <- function(endpoint = endpoint, collection = "Rainfall", from = "", to = "", month = "") {
  get_data_collection(endpoint = endpoint, collection = collection, method = "Total", time_interval = NA, from = from, to = to, interval = "1 month", alignment = "00:00") %>%
    rename(rainfall_total = value) %>%
    group_by(site) %>%
    arrange(site, datetime) %>%
    # slice(-1) %>% # remove first row due to offset, datetime refers to start of interval.
    ungroup() %>%
    mutate(
      interval = months(1),
      rainfall_total = round(rainfall_total, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total)) %>% 
    group_by(site, month) %>% 
    summarise(
      rainfall_avg_total = mean(rainfall_total, na.rm = TRUE),
      rainfall_med_total = median(rainfall_total, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    filter(month == !!month)
}


start_date <- month_year
end_date <- month_year + months(1)
month <- format(start_date, "%b")

now <- format(Sys.time(), "%Y%m%dT%H%M%S")
now_plot <- str_replace(now, "T", " ")

elevation_pts <- read_csv("data/elevation_pts.csv") %>% 
  rename(
    easting = X,
    northing = Y,
    elevation = New_Zealand_Elevation
  ) %>% 
  mutate(x = easting, y = northing)

nelsontasman <- st_read("data/nelsontasman.gpkg", layer = "nelsontasman") 

tdc_endpoint <- "http://envdata.tasman.govt.nz/data.hts?"
mdc_endpoint <- "http://hydro.marlborough.govt.nz/mdc data.hts?"
wgrc_endpoint <- "http://hilltop.wcrc.govt.nz/Websitedata.hts?"

tdc_sites <- get_site_data(endpoint = tdc_endpoint) %>% 
  mutate(region = "nelsontasman")
mdc_sites <- get_site_data(endpoint = mdc_endpoint) %>% 
  mutate(region = "marlborough")
wgrs_sites <- get_site_data(endpoint = wgrc_endpoint) %>% 
  mutate(region = "westcoast")
sites <- bind_rows(tdc_sites, mdc_sites, wgrs_sites)

# Rainfall month total
from <- format(start_date, "%Y%m%d")
to <- format(end_date, "%Y%m%d")

tdc_month_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "Rainfall", from = from, to = to, month = month)
mdc_month_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = from, to = to, month = month)
wgrs_month_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = from, to = to, month = month)

month_rainfall <- bind_rows(tdc_month_rainfall, mdc_month_rainfall, wgrs_month_rainfall) %>% 
  dplyr::select(-c(month, rainfall_med_total)) %>% 
  dplyr::rename(rainfall_total = rainfall_avg_total)

rainfall_sites <- tibble(site = unique(month_rainfall$site))

# Rainfall month total average last 5 years
from <- format(start_date - years(5), "%Y%m%d")
to <- format(end_date - years(1), "%Y%m%d")

tdc_month_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "Rainfall", from = "Data Start", to = "Data End", month = month)
mdc_month_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = "Data Start", to = "Data End", month = month)
wgrs_month_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = "Data Start", to = "Data End", month = month)

average5yr_month_rainfall <- bind_rows(tdc_month_rainfall, mdc_month_rainfall, wgrs_month_rainfall) %>% 
  dplyr::select(-c(month, rainfall_med_total)) %>% 
  dplyr::rename(rainfall_total_avg_past5years = rainfall_avg_total)

# Rainfall historic month total median
tdc_month_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "Rainfall", from = "Data Start", to = "Data End", month = month)
mdc_month_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = "Data Start", to = "Data End", month = month)
wgrs_month_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = "Data Start", to = "Data End", month = month)

medianhistoric_month_rainfall <- bind_rows(tdc_month_rainfall, mdc_month_rainfall, wgrs_month_rainfall) %>% 
  dplyr::select(-c(month, rainfall_avg_total)) %>% 
  dplyr::rename(rainfall_total_medianhistoric = rainfall_med_total)

rainfall <- rainfall_sites %>%
  left_join(sites, by = "site") %>%
  left_join(month_rainfall, by = "site") %>%
  left_join(average5yr_month_rainfall, by = "site") %>% 
  left_join(medianhistoric_month_rainfall, by = "site") %>% 
  mutate(
    historic_ratio = rainfall_total / rainfall_total_medianhistoric,
    summary_at = now
  )

rainfall <- rainfall %>%
  st_drop_geometry() %>%
  dplyr::select(-geometry) %>% 
  drop_na() 
rainfall %>% 
  write.csv(glue('outputs/{format(month_year, "%Y%m")}_tots_rainfall_summary.csv'))

####### ISOHYET through Co-Kriging
# https://zia207.github.io/geospatial-r-github.io/cokriging.html

rainfall <- rainfall %>% 
  mutate(x = easting, y = northing)

r <- raster("data/tots_elevation.tif")
rainfall$elevation <- extract(r, rainfall[c("easting", "northing")], sp = T)

coordinates(rainfall) <- ~x + y
crs(rainfall) <- CRS("+init=epsg:2193")
coordinates(elevation_pts) <- ~x + y
crs(elevation_pts) <- CRS("+init=epsg:2193")

powerTransform(rainfall$rainfall_total)

rainfall_total.bc <- bcPower(rainfall$rainfall_total, -0.2255934)
rainfall$rainfall_total.bc <- bcPower(rainfall$rainfall_total, -0.2255934)

# create a data.frame
co.var <- data.frame(rainfall[, c("rainfall_total", "elevation")])
df.cor <- cbind(rainfall_total.bc, co.var)
# Corelation matrix
cor.matrix <- rcorr(as.matrix(df.cor))
cor.matrix 

# Direct Varoigram of Target Variables (rainfall_total)
# Variogram
v.rainfall_total <- variogram(rainfall_total.bc ~ 1, data = rainfall, cloud = F)
# Intial parameter set by eye esitmation
m.rainfall_total <- vgm(1.5, "Exp", 400000, 0.5)
# least square fit
m.f.rainfall_total <- fit.variogram(v.rainfall_total, m.rainfall_total)
p1 <- plot(v.rainfall_total, pl=F, model = m.f.rainfall_total, main = "rainfall_total")


# Direct Varoigram of Variogram Modeling of Co-Variables (elevation)
# Variogram
v.elevation <- variogram(elevation~ 1, data = rainfall, cloud = F)
# Intial parameter set by eye esitmation
m.elevation <- vgm(1.5,"Exp", 4000, 1)
# least square fit
m.f.elevation <- fit.variogram(v.elevation, m.elevation)
p2 <- plot(v.elevation, pl=F, model=m.f.elevation, main = "elevation")

grid.arrange(p1, p2, ncol = 2)  # Multiplot 

g <- gstat(NULL, id = "rainfall_total", form = rainfall_total.bc ~ 1, data = rainfall)
g <- gstat(g, id = "elevation", form = elevation ~ 1, data = rainfall)

v.cross <- variogram(g)
plot(v.cross, pl=F)

g <- gstat(g, id = "rainfall_total", model = m.f.rainfall_total, fill.all=T)

g <- fit.lmc(v.cross, g)

plot(variogram(g), model=g$model)

# Co-Kriging Prediction at grid locations
CK <- predict(g, elevation_pts)
summary(CK)

# Back transformation
k1 <- 1/-0.2255934                                   
CK$CK.pred <-((CK$rainfall_total.pred * -0.2255934+1) ^ k1)
CK$CK.var <-((CK$rainfall_total.var * -0.2255934+1) ^ k1)
summary(CK)

# Convert to raster
CK.pred <- rasterFromXYZ(as.data.frame(CK)[, c("x", "y", "CK.pred")])
CK.var <- rasterFromXYZ(as.data.frame(CK)[, c("x", "y", "CK.var")])

p3 <- ggR(CK.pred, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("CK Predicted rainfall_total")+
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggR(CK.var, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("blue",  "green","yellow", "orange"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("CK Prediction Variance")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p3, p4, ncol = 2)  # Multiplot

# save raster
out_r <- raster::mask(x = CK.pred, mask = nelsontasman)
writeRaster(out_r, glue('outputs/{format(month_year, "%Y%m")}_tots_rainfall_summary.tiff'), overwrite = TRUE)

# produce output for reporting 
# points on map, 




#rm(list = ls())