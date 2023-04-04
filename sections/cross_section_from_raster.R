library(sp)
library(sf)
library(raster)
library(tidyverse)
library(tmap)
library(gridExtra)

distance_along_cs <- function(x, y, x0, y0) {
  # Function to calculate distance along cross-section from origin.
  return(sqrt((x - x0)^2 + (y - y0)^2))
}

dem <- raster("data/awaroa.tif")

start <- c(1568544.83, 5491213.26) 
end <- c(1568770.08, 5491226.85)  

ls <- st_sfc(st_linestring(rbind(start, end)), crs = st_crs(2193))

pts = ls %>% 
  st_sample(25) %>% 
  st_cast("POINT") %>% 
  st_sf()  %>% 
  mutate(
    x = unlist(map(.$geometry,1)),
    y = unlist(map(.$geometry,2))
  ) %>% 
  arrange(x)

pts <- pts %>% 
  mutate(
    d_cs = mapply(distance_along_cs, x, y, .$x[1], .$y[1])
  )

plot(dem)
plot(pts, add = TRUE)

pts$elevation <- raster::extract(dem, pts)

tm <- tm_shape(dem) +
  tm_raster() +
  tm_shape(ls) +
  tm_lines(lwd = 3) 
p1 <- tmap_grob(tm)

p2 <- ggplot(pts) +
  geom_line(aes(d_cs, elevation)) +
  geom_point(head(pts ,1), mapping = aes(d_cs, elevation)) +
  geom_point(tail(pts ,1), mapping = aes(d_cs, elevation), shape = 8) +
  labs(y = "elevation (m NZVD")

grid.arrange(p1, p2, ncol = 1)
