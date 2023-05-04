library(tdcR)
library(tidyverse)
library(sp)
library(raster)
library(glue)

# Get rainfall sites
sites <- tdcR::get_sites(collection = "AllRainfall", latlong = FALSE)

# Extract hirds values at site locations
extract_hirds <- function(r_name, sites) {
  print(r_name)
  m_name <- gsub(".*duration(.+).tif", "\\1", r_name)

  duration <- gsub(".*duration(.+)_ARI.*", "\\1", r_name)
  ari <- gsub(".*ARI(.+).tif", "\\1", r_name)

  r <- raster(glue("hirdsv4/{r_name}"))
  r <- projectRaster(r, crs = 2193) # re-project to NZTM

  sites$val <- extract(r, sites[c("easting", "northing")], sp = T)
  sites %>%
    mutate(
      hirds = m_name,
      duration = duration,
      ari = ari
    ) %>%
    dplyr::select(c(site, hirds, duration, ari, val))
}

hirds_files <- list.files(path = "hirdsv4", pattern = "*.tif")
hirds_res <- do.call("rbind", lapply(hirds_files, extract_hirds, sites))

saveRDS(hirds_res, "processing/hirds_tdc_sites.rds")
