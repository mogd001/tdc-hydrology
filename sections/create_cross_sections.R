library(tidyverse)
library(ggplot2)
library(sf)
library(sp)
library(patchwork)

#mapping
library(ggmap)

transform_to_cs <- function(x, y, intcpt, slope) {
  # Function to transform x, y coordinate to point perpindicular to line. 
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
  
  data.frame(x_dash, y_dash)
}


distance_along_cs <- function(x, y, x0, y0) {
  # Function to calculate distance along cross-section from origin.
  d <- sqrt((x - x0)^2 + (y - y0)^2)
}

###### INPUTS
#z_offset <- 16.6651 # if using lat-long, for ellipsoid to nzvd2016 conversion
zoom <- 18 # for imagery

site <- "Borck Creek"
cs_name <-  "BCCS" # ref column
data_fp <- "data/20220720_borck_creek_example.csv"

#site <- "Motueka at Norths Bridge" # Tapawera Bridge # Norths Bridge # Korere Bridge
#cs_name <- "Norths_CS" # Tapawera_CS # Norths_CS # Korere_CS
#data_fp <- "data/20220729_waterwatchradars_rivers.csv"
###### 

df <- read_csv(data_fp) %>% 
  select_all(~gsub("\\s+|\\.", "_", .)) %>% 
  rename_all(tolower) %>% 
  mutate(
    lat = latitude,
    lon = longitude
  )

#TODO - 3D transformation to handle Z coordinate.
if (all(is.na(df$easting))) {
  # perform wgs -> nztm conversion, include offset.
  print("WGS coordinates. Transform.")
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
  filter(ref == !!cs_name) %>% 
  select(easting, northing, elevation, description, lat, lon)

# fit linear regression to eastings/northings
mdl <- lm(northing ~ easting, data = cs)
intcpt <- unname(coefficients(mdl)[1])
slope <- unname(coefficients(mdl)[2])

# calculate distance along cross-section
cs <- cs %>% 
  mutate(
    unnest(as_tibble(t(mapply(transform_to_cs, easting, northing, intcpt = intcpt, slope = slope))), cols = c(x_dash, y_dash))
  ) %>% 
  mutate(
    d_cs = mapply(distance_along_cs, x_dash, y_dash, .$x_dash[1], .$y_dash[1])
  )

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

# draw map
centre <- c((min(cs$lon) + max(cs$lon))/2, (min(cs$lat) + max(cs$lat))/2) # middle of cs

mp <- slice(cs_plot, round((1 + nrow(cs_plot))/2,0))
arrow <- cs_plot %>% 
  st_drop_geometry() %>% 
  slice(1) %>% 
  add_row(lon = mp$lon, lat = mp$lat) 
basemap <- get_googlemap(centre, zoom, maptype = "satellite")

p1 <- ggmap(basemap) +
  geom_point(cs, mapping = aes(lon, lat), size = 2, shape = 3, color = "#ed7014", alpha = 0.8) +
  geom_line(cs_plot, mapping = aes(lon, lat), color = "#ff0000") + 
  geom_path(arrow, mapping = aes(lon, lat), color = "#ff0000", arrow = arrow(), alpha = 0.8) + 
  geom_point(cs_plot, mapping = aes(lon, lat), color = "#ff0000") + 
  scale_x_continuous(limits = c(min(cs$lon) - 0.00005, max(cs$lon) + 0.00005), expand = c(0, 0)) +
  scale_y_continuous(limits = c( min(cs$lat) - 0.00005, max(cs$lat) + 0.00005), expand = c(0, 0)) + 
  theme(axis.title.x=element_blank(), # axis remove labels
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# draw cross-section
p2 <- ggplot(cs, aes(d_cs, elevation), color = "blue") + 
  geom_line(color = "#ff0000") +
  geom_point(data = cs, aes(d_cs, elevation), color = "red", size = 2, alpha = 0.8) +
  coord_fixed(3) + 
  labs(x = "Distance along Cross-section (m)", y = "Elevation (m NZVD2016)") + 
  theme_bw()

p3 <- p1 / p2 + plot_annotation(
  title = paste0("Cross-section ", site),
  caption = "Source: TDC Hydrology"
)

cs %>% 
  st_drop_geometry() %>% 
  write_csv(paste0("outputs/cs-", site, ".csv"))

print(p3)
dev.off()
