library(tidyverse)
library(ggplot2)
library(sf)
library(sp)
library(patchwork)
library(glue)
library(ggmap) # mapping

###### INPUTS ###############################################
site <- "Waimea at TDC Nursery"
cs_name <-  "cs" # "ref" column - will need to add to input csv.

water_level <- 6.1 # water level to plot.
water_level_additional_text <- "at 2022-08-20 11:15"

rtk_cross_section <- read_csv("data/20220820_waimea_survey.csv")

river_start_point <- "cs_adcpstart" # set to NA if no adcp not used to complete cross-section
river_cross_section <- read_csv("data/20220829_waimea_adcp_profile.csv") %>% 
  rename(
    d_cs = Length,
    elevation = Height
  )

z_offset <- NA # if using lat-long, for ellipsoid to nzvd2016 conversion, preference is to manage all coordinates in NZTM and NZVD2016. 
zoom <- 18 # for downloaded imagery, default = 18
############################################################# 


transform_to_cs <- function(x, y, intcpt, slope) {
  # Function t transform x, y coordinate to point perpendicular to line. 
  # line defined by intercept : intcpt and slope : slope.
  
  theta <- abs(atan(slope))
  l = abs(y - (slope * x + intcpt)) * sin(theta)
  
  y_atx <- slope * x + intcpt 
  
  delta_x <- l * cos(theta)
  delta_y <- l * sin(theta)
  
  if (slope >= 0) {
    if (y >= y_atx) {
      x_dash <- x + delta_x
      y_dash <- y_atx + delta_y
    } else {
      x_dash <- x - delta_x 
      y_dash <- y_atx - delta_y
    }
  } else {
    if (y > y_atx) {
      x_dash <- x - delta_x 
      y_dash <- y_atx + delta_y
    } else {
      x_dash <- x + delta_x 
      y_dash <- y_atx - delta_y
    }
  }
  
  return(data.frame(x_dash, y_dash))
}


distance_along_cs <- function(x, y, x0, y0) {
  # Function to calculate distance along cross-section from origin.
  return(sqrt((x - x0)^2 + (y - y0)^2))
}


transform_river_cross_section <- function(cs_start, river_cs_start, river_cross_section, intcpt, slope) {
  # Function to transform river cross-section (measured through adcp for example) to real world coordinates
  # for combining with RTK survey cross-section.
  theta <- atan(slope)
  
  x0 <- river_cs_start$x_dash
  y0 <- river_cs_start$y_dash
  
  xs <- cs_start$x_dash
  ys <- cs_start$y_dash
  
  d_csstart <- sqrt((xs - x0)^2 + (ys - y0)^2)
  
  # resample river_cross_section (10% of points)
  river_cross_section <- river_cross_section %>% 
    mutate(
      filt = (row_number() - 1) %% 10
    ) %>% 
    filter(filt == 0) %>% 
    select(-filt) 
  
  river_cross_section$x_dash <- x0 + river_cross_section$d_cs * cos(theta)
  river_cross_section$y_dash <- y0 + river_cross_section$d_cs * sin(theta)
  
  river_cross_section %>% 
    mutate(
      ref = "river_cross_section",
      easting = x_dash,
      northing = y_dash
    ) %>% 
    st_as_sf(coords = c("easting", "northing"), crs = 2193) %>% 
    mutate(
      easting = st_coordinates(.)[, "X"],
      northing = st_coordinates(.)[, "Y"],
      elevation = as.numeric(elevation)
    ) %>% 
    st_transform(crs = 4326) %>% 
    mutate(
      lon = st_coordinates(.)[, "X"],
      lat = st_coordinates(.)[, "Y"]
    ) %>% 
    st_transform(crs = 2193)  %>% 
    mutate(
      d_cs = d_cs + d_csstart # update from cross-section start
    ) %>% 
    slice(-1) # drop first row as covered by survey
}

now <- format(Sys.time(), "%Y%m%dT%H%M%S")
now_plot <- str_replace(now, "T", " ")

df <- rtk_cross_section %>% 
  select_all(~gsub("\\s+|\\.", "_", .)) %>% 
  rename_all(tolower) %>% 
  mutate(
    lat = latitude,
    lon = longitude
  )

#TODO - 3D transformation to handle Z coordinate.  Preferentially everything is handled in NZTM/NZVD2016.
if (all(is.na(df$easting))) {
  # perform wgs -> nztm conversion, include offset.
  print("WGS coordinates. Transform.  Has z_offset been correctly defined?")
  df <- df %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
    st_transform(crs = 2193) %>% 
    mutate(
      easting = st_coordinates(.)[, "X"],
      northing = st_coordinates(.)[, "Y"],
      elevation = as.numeric(ellipsoidal_height) - z_offset
    )
} else {
  print("NZTM coordinates.")
  df <- df %>% 
    st_as_sf(coords = c("easting", "northing"), crs = 2193) %>% 
    mutate(
      easting = st_coordinates(.)[, "X"],
      northing = st_coordinates(.)[, "Y"],
      elevation = as.numeric(elevation)
    )
}

###### CROSS SECTION
cs <- df %>% 
  filter(grepl(!!cs_name, ref)) %>% 
  select(easting, northing, elevation, ref, description, lat, lon, name)

# fit linear regression to eastings/northings
mdl <- lm(northing ~ easting, data = cs)
intcpt <- unname(coefficients(mdl)[1])
slope <- unname(coefficients(mdl)[2])

# calculate distance along cross-section
cs <- cs %>% 
  mutate(
    unnest(as_tibble(t(mapply(transform_to_cs, easting, northing, intcpt = intcpt, slope = slope))), cols = c(x_dash, y_dash))
  ) %>% 
  arrange(x_dash) %>%
  mutate(
    d_cs = mapply(distance_along_cs, x_dash, y_dash, .$x_dash[1], .$y_dash[1])
  ) %>% 
  relocate(elevation, .after = d_cs)

# update cross-section to include river section from adcp (or other source)
if (!is.na(river_start_point)) {
  cs_start <- cs %>% head(1)
  river_cs_start <- cs %>% filter(ref == !!river_start_point)
  river_cross_section <- transform_river_cross_section(cs_start, river_cs_start, river_cross_section, intcpt = intcpt, slope = slope)
  
  cs <- bind_rows(cs, river_cross_section) %>% 
    arrange(d_cs)
}

###### Visualising and Exporting
cs_plot <- tibble(easting = cs$x_dash, northing = cs$y_dash) %>% 
  st_as_sf(coords = c("easting", "northing"), crs = 2193) %>% 
  st_transform(crs = 4326) %>% 
  mutate(
    lon = st_coordinates(.)[, "X"],
    lat = st_coordinates(.)[, "Y"],
  )

# for development
p0 <- ggplot(cs, aes(easting, northing)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(data = cs, aes(x_dash, y_dash), color = "blue", size = 4, alpha = 0.2) +
  coord_fixed(ratio = 1)
p0

# draw map
centre <- c((min(cs$lon) + max(cs$lon))/2, (min(cs$lat) + max(cs$lat))/2) # middle of cs

mp <- slice(cs_plot, round((1 + nrow(cs_plot))/2,0))
arrow <- cs_plot %>% 
  st_drop_geometry() %>% 
  slice(1) %>% 
  add_row(lon = mp$lon, lat = mp$lat) 

basemap <- get_googlemap(centre, zoom, maptype = "satellite")
basemap_attributes <- attributes(basemap)
basemap_transparent <- matrix(adjustcolor(basemap, 
                                                alpha.f = 0.5), 
                                    nrow = nrow(basemap))
attributes(basemap_transparent) <- basemap_attributes

p1 <- ggmap(basemap_transparent) +
  geom_point(cs, mapping = aes(lon, lat), size = 2, shape = 3, color = "#ed7014", alpha = 0.8) +
  geom_text(cs, mapping = aes(label = name), size = 3, hjust = -0.25, color = "#ed7014", angle = 45) + 
  geom_line(cs_plot, mapping = aes(lon, lat), color = "brown") + 
  geom_path(arrow, mapping = aes(lon, lat), color = "brown", arrow = arrow(), alpha = 0.8) + 
  geom_point(cs_plot, mapping = aes(lon, lat), color = "brown") + 
  scale_x_continuous(limits = c(min(cs$lon) - 0.00015, max(cs$lon) + 0.00015), expand = c(0, 0)) +
  scale_y_continuous(limits = c( min(cs$lat) - 0.00015, max(cs$lat) + 0.00015), expand = c(0, 0)) + 
  theme(axis.title.x=element_blank(), # axis remove labels
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# draw cross-section
plot_water_level = FALSE
if (!is.na(water_level)) {
  plot_water_level = TRUE
  
  wl_x_start <- 0
  wl_x_end <- max(cs$d_cs)
  
  wl_symbol <- tibble(x = 0.5 * (wl_x_start + wl_x_end), y = water_level + 0.4, label = glue("Water level {water_level} m {water_level_additional_text}"))
}

spline_int <- as.data.frame(spline(cs$d_cs, cs$elevation)) # if wanting a a spline connecting the datapoints opposed to straight lines

p2 <- ggplot(cs, aes(d_cs, elevation)) + 
  geom_point(color = "brown", size = 2, alpha = 0.8) +
  geom_line(color = "brown") + #data = spline_int, aes(x, y)
  geom_text(aes(label  = name), size = 2, hjust = -0.5, vjust = -1, color = "#ed7014", angle = 45) + 
  coord_fixed(10) + 
  scale_y_continuous(expand = expansion(add = 2)) + 
  labs(x = "Distance along Cross-section (m)", y = glue("Elevation (m NZVD2016)")) + 
  theme_bw() +
  {if(plot_water_level) geom_segment(aes(x = wl_x_start, xend = wl_x_end, y = water_level, yend = water_level), color = "blue", alpha = 0.8)} + 
  {if(plot_water_level) geom_point(wl_symbol, mapping = aes(x, y), shape = 25, fill = "blue", color = "black", size = 3)} + 
  {if(plot_water_level) geom_text(wl_symbol, mapping = aes(x, y, label = label), hjust = -0.2,  color = "blue", size = 3)}

p3 <- p1 / p2 + plot_annotation(
  title = paste0("Cross-section ", site),
  caption = glue("Source: TDC Hydrology, compiled {now_plot}")
)

# save plot
print(p3)
ggsave(paste0("outputs/cs-", site, ".png"), dpi = 600, width = 6, height = 10)
dev.off()

# save cross section data
cs %>% 
  st_drop_geometry() %>% 
  write_csv(paste0("outputs/cs-", site, ".csv"))
