library(tidyverse)
library(lubridate)
library(glue)
library(tdcR)
library(sf)
library(plotly)
library(ggmap)
library(gstat)
library(automap)
library(raster)
library(car)
library(classInt)
library(RStoolbox)
library(spatstat)
library(dismo)
library(fields)
library(gridExtra)
library(Hmisc)
library(patchwork)
library(tmaptools)
library(comprehenr)

########### INPUTS ###########
ms <- seq(1, 5, 1)
#ms <- seq(8, 8, 1)

month_years <- to_vec(for (m in ms) paste0("2023-", m))
########### #################

# TODO - functionalise kriging

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

get_rainfall_monthly_data <- function(endpoint = endpoint, collection = "AllRainfall", from = "", to = "", month = "") {
  get_data_collection(
    endpoint = endpoint, collection = collection, method = "Total", interval = "1 months",
    from = from, to = to
  ) %>%
    mutate(datetime = datetime - months(1),
           month = month(datetime, label = TRUE)) %>% 
    rename(rainfall_total = value) %>%
    group_by(site) %>%
    arrange(site, datetime) %>%
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

grid <- read_csv("data/elevation_pts.csv") %>%
  rename(
    easting = X,
    northing = Y,
    elevation = New_Zealand_Elevation
  )

coordinates(grid) <- ~ easting + northing
crs(grid) <- CRS("+init=epsg:2193")

nelsontasman <- st_read("data/context.gpkg", layer = "nelsontasman")
context <- st_read("data/context.gpkg", layer = "towns", query = "SELECT * FROM \"towns\" WHERE name <> 'Brightwater'")

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

for (month_year in month_years) {
  month_year <- ym(month_year)
  print(month_year)

  start_date <- month_year
  end_date <- month_year + months(1)
  month <- format(start_date, "%b")

  now <- format(Sys.time(), "%Y%m%dT%H%M")
  now_plot <- str_replace(now, "T", " ")
  now_plot <- glue("{substr(now_plot, 1, nchar(now_plot) - 1)}0") # update text to nearest 10 minutes

  # Rainfall month total
  from <- format(start_date, "%Y%m%d")
  to <- format(end_date, "%Y%m%d")

  tdc_month_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "AllRainfall", from = from, to = to, month = month)
  mdc_month_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = from, to = to, month = month)
  wgrs_month_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = from, to = to, month = month)

  month_rainfall <- dplyr::bind_rows(tdc_month_rainfall, mdc_month_rainfall, wgrs_month_rainfall) %>%
    dplyr::select(-c(month, rainfall_med_total)) %>%
    dplyr::rename(rainfall_total = rainfall_avg_total)

  # Rainfall month total average last 5 years
  from <- format(start_date - years(5), "%Y%m%d")
  to <- format(end_date - years(1), "%Y%m%d")

  tdc_month_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "AllRainfall", from = "Data Start", to = "Data End", month = month)
  mdc_month_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = "Data Start", to = "Data End", month = month)
  wgrs_month_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = "Data Start", to = "Data End", month = month)

  average5yr_month_rainfall <- dplyr::bind_rows(tdc_month_rainfall, mdc_month_rainfall, wgrs_month_rainfall) %>%
    dplyr::select(-c(month, rainfall_med_total)) %>%
    dplyr::rename(rainfall_total_avg_past5years = rainfall_avg_total)

  # Rainfall historic month total median
  tdc_month_rainfall <- get_rainfall_monthly_data(endpoint = tdc_endpoint, collection = "AllRainfall", from = "Data Start", to = "Data End", month = month)
  mdc_month_rainfall <- get_rainfall_monthly_data(endpoint = mdc_endpoint, collection = "Rainfall2", from = "Data Start", to = "Data End", month = month)
  wgrs_month_rainfall <- get_rainfall_monthly_data(endpoint = wgrc_endpoint, collection = "WebRainfall", from = "Data Start", to = "Data End", month = month)

  medianhistoric_month_rainfall <- dplyr::bind_rows(tdc_month_rainfall, mdc_month_rainfall, wgrs_month_rainfall) %>%
    dplyr::select(-c(month, rainfall_avg_total)) %>%
    dplyr::rename(rainfall_total_medianhistoric = rainfall_med_total)

  rainfall_sites <- tibble(site = unique(month_rainfall$site))

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
    drop_na() %>% 
    subset(site != "Red Hills")
  rainfall %>%
    write.csv(glue('outputs/{format(month_year, "%Y%m")}_tots_rainfall_summary.csv'))

  ####### ISOHYET through Kriging/Co-Kriging
  # https://zia207.github.io/geospatial-r-github.io/cokriging.html
  rainfall <- rainfall %>%
    mutate(
      rainfall_total = if_else(rainfall_total <= 0, 0.0001, rainfall_total), # account for 0 rainfall months
      x = easting,
      y = northing
    )

  r1 <- raster("data/tots_elevation.tif")
  r2 <- raster("data/average-annual-rainfall-19722016_1.tif")
  rainfall$elevation <- extract(r1, as_tibble(rainfall)[c("easting", "northing")], sp = T)
  rainfall$rainfall_annual <- extract(r2, as_tibble(rainfall)[c("easting", "northing")], sp = T)

  coordinates(rainfall) <- ~ x + y
  crs(rainfall) <- CRS("+init=epsg:2193")

  pt_val <- powerTransform(rainfall$rainfall_total)$lambda
  # powerTransform(rainfall$rainfall_annual)

  rainfall_total.bc <- bcPower(rainfall$rainfall_total, pt_val)
  rainfall$rainfall_total.bc <- bcPower(rainfall$rainfall_total, pt_val)

  co.var <- data.frame(rainfall[, c("rainfall_total", "elevation", "rainfall_annual")])
  df.cor <- cbind(rainfall_total.bc, co.var)
  # Correlation matrix
  cor.matrix <- rcorr(as.matrix(df.cor))
  cor.matrix

  # Direct Varoigram of Target Variables (rainfall_total)
  v.rainfall_total <- variogram(rainfall_total.bc ~ 1, data = rainfall, cloud = F)
  plot(v.rainfall_total)

  # Initial parameter set by eye estimation
  m.rainfall_total <- vgm(psill = max(v.rainfall_total$gamma) * 0.9, model = "Exp", range = max(v.rainfall_total$dist) / 2, nugget = mean(v.rainfall_total$gamma) / 4)

  # Least square fit
  m.f.rainfall_total <- fit.variogram(v.rainfall_total, m.rainfall_total)

  p1 <- plot(v.rainfall_total, pl = F, model = m.f.rainfall_total, main = "rainfall_total")
  p1

  # Direct Varoigram of Variogram Modeling of Co-Variables (elevation)
  # Variogram
  v.elevation <- variogram(elevation ~ 1, data = rainfall, cloud = F)
  # Initial parameter set by eye estimation
  m.elevation <- vgm(psill = max(v.elevation$gamma) * 0.9, model = "Exp", range = max(v.elevation$dist) / 2, nugget = mean(v.elevation$gamma) / 4)
  # least square fit
  m.f.elevation <- fit.variogram(v.elevation, m.elevation)

  p2 <- plot(v.elevation, pl = F, model = m.f.elevation, main = "elevation")
  p2

  # Direct Varoigram of Variogram Modeling of Co-Variables (annual rainfall)
  # Variogram
  v.rainfall_annual <- variogram(rainfall_annual ~ 1, data = rainfall, cloud = F)
  # Initial parameter set by eye estimation
  m.rainfall_annual <- vgm(psill = max(v.rainfall_annual$gamma) * 0.9, model = "Exp", range = max(v.rainfall_annual$dist) / 2, nugget = mean(v.rainfall_annual$gamma) / 4)
  # least square fit
  m.f.rainfall_annual <- fit.variogram(v.rainfall_annual, m.rainfall_annual)

  p3 <- plot(v.rainfall_annual, pl = F, model = m.f.rainfall_annual, main = "rainfall_annual")
  p3

  grid.arrange(p1, p2, p3, ncol = 3) # Multiplot

  # TODO GET COKRIGING WORKING!
  g <- gstat(NULL, id = "rainfall_total", form = rainfall_total.bc ~ 1, data = rainfall)
  # g <- gstat(g, id = "elevation", form = elevation ~ 1, data = rainfall)
  # g <- gstat(g, id = "rainfall_annual", form = rainfall_annual ~ 1, data = rainfall)

  v.cross <- variogram(g)
  plot(v.cross, pl = F)

  g <- gstat(g, id = "rainfall_total", model = m.f.rainfall_total, fill.all = T)
  g <- fit.lmc(v.cross, g)
  plot(variogram(g), model = g$model)

  # Co-Kriging Prediction at grid locations
  Sys.sleep(2)
  ck <- stats::predict(g, grid)

  k1 <- 1 / pt_val
  ck$ck.pred <- ((ck$rainfall_total.pred * pt_val + 1)^k1)
  ck$ck.var <- ((ck$rainfall_total.var * pt_val + 1)^k1)
  summary(ck)

  # Convert to raster
  ck.pred <- rasterFromXYZ(as.data.frame(ck)[, c("easting", "northing", "ck.pred")])
  ck.var <- rasterFromXYZ(as.data.frame(ck)[, c("easting", "northing", "ck.var")])

  p3 <- ggR(ck.pred, geom_raster = TRUE) +
    scale_fill_gradientn("", colours = c("orange", "yellow", "green", "sky blue", "blue")) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ggtitle("CK Predicted rainfall_total") +
    theme(plot.title = element_text(hjust = 0.5))

  p4 <- ggR(ck.var, geom_raster = TRUE) +
    scale_fill_gradientn("", colours = c("blue", "green", "yellow", "orange")) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ggtitle("CK Prediction Variance") +
    theme(plot.title = element_text(hjust = 0.5))

  out1 <- p3 + p4
  out1

  # save raster
  out_r <- raster::mask(x = ck.pred, mask = nelsontasman)
  raster::crs(out_r) <- "EPSG:2193"
  writeRaster(out_r, glue('outputs/{format(month_year, "%Y%m")}_tots_rainfall_summary.tif'), overwrite = TRUE)

  ####### Visualisation
  basemap <- get_map(location = c(lon = 172.83031417, lat = -41.40166015), maptype = "terrain-background", zoom = 8, alpha = 0.3) # Richmond, NZ (alternative for getting location)

  pts <- as_tibble(rainfall)
  # max_rainfall <- max(pts$rainfall_total, na.rm = TRUE)
  max_rainfall <- 1000
  out_wgs84 <- out_r %>%
    projectRaster(crs = 4326)

  plot_r <- as_tibble(as.data.frame(out_wgs84, xy = TRUE)) %>%
    rename(ck_rainfall = ck.pred) %>%
    mutate(ck_rainfall = ifelse(ck_rainfall > max_rainfall, max_rainfall, ck_rainfall))

  pts <- pts %>% mutate(
    rainfall_total_capped = if_else(rainfall_total >= 1000, 1000, rainfall_total)
  )

  nelsontasman_wgs84 <- st_transform(nelsontasman, crs = 4326)
  bb <- st_bbox(nelsontasman_wgs84)
  context_wgs84 <- st_transform(context, crs = 4326) %>%
    mutate(
      lon = st_coordinates(.)[, "X"],
      lat = st_coordinates(.)[, "Y"]
    )

  p5 <- ggmap(basemap, darken = c(0.6, "white")) +
    coord_cartesian() +
    geom_sf(nelsontasman_wgs84, mapping = aes(), fill = NA, color = "white", inherit.aes = FALSE) + # note: geom_sf sets to correct aspect ratio
    geom_point(pts, mapping = aes(x = longitude, y = latitude, size = rainfall_total, color = rainfall_total_capped)) +
    geom_text(pts, mapping = aes(x = longitude, y = latitude, label = round(rainfall_total, 0)), fontface = "bold", color = "white", size = 1.75, alpha = 1.0) +
    coord_sf(xlim = c(bb$xmin - 0.3, bb$xmax + 0.2), ylim = c(bb$ymin - 0.2, bb$ymax + 0.2)) +
    labs(color = "Rainfall Total (mm)") +
    theme_bw() +
    scale_color_viridis(option = "turbo", limits = c(0, max_rainfall), alpha = 0.7) +
    scale_size_continuous(range = c(4, 8)) +
    guides(size = "none") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_blank(),
      legend.position = c(0.15, 0.9)
    )

  p6 <- ggmap(basemap, darken = c(0.6, "white")) +
    coord_cartesian() +
    geom_raster(plot_r, mapping = aes(x = x, y = y, fill = ck_rainfall)) +
    geom_point(pts, mapping = aes(x = longitude, y = latitude), size = 0.5, alpha = 0.4, color = "black", shape = 3) +
    geom_sf(nelsontasman_wgs84, mapping = aes(), fill = NA, color = "white", inherit.aes = FALSE) +
    geom_sf(context_wgs84, mapping = aes(), fill = NA, shape = 2, size = 2.5, color = "black", inherit.aes = FALSE) +
    geom_text(context_wgs84, mapping = aes(lon, lat, label = name), size = 3.5, color = "black", hjust = -0.18, fontface = "bold") +
    coord_sf(xlim = c(bb$xmin - 0.3, bb$xmax + 0.2), ylim = c(bb$ymin - 0.2, bb$ymax + 0.2)) +
    labs(fill = "Rainfall Total (mm)") +
    theme_bw() +
    scale_fill_viridis(option = "turbo", limits = c(0, max_rainfall), na.value = "transparent", alpha = 0.7) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_blank(),
      legend.position = c(0.15, 0.9)
    )

  theme_border <- theme_gray() +
    theme(
      plot.background = element_rect(fill = NA, colour = "#273691", size = 3),
      plot.title = element_text(size = 20, hjust = 0.5, vjust = -0.75, face = "bold", colour = "#273691"),
      plot.caption = element_text(size = 7, face = "italic", color = "black")
    )

  out2 <- p5 + p6 +
    plot_annotation(
      title = glue('Monthly Rainfall Summary {format(month_year, "%B %Y")}'),
      caption = glue("TDC Environmental Data"), # compiled {now_plot}
      theme = theme_border
    )

  ggsave(glue('outputs/{format(month_year, "%Y%m")}.png'), width = 12, height = 9.085, plot = out2, dpi = 300) # note aspect ratio
}