library(tdcR) # hilltop server commands
library(tidyverse)
library(sp)
library(raster)
library(glue)

# Get rainfall sites with x,y - nztm
rainfall_sites <- tdcR::get_collections() %>%
  filter(collection == "Rainfall")

sites <- tdcR::get_sites(latlong = FALSE) %>%
  subset(site %in% rainfall_sites$site)

# Extract hirds values at site locations
extract_hirds <- function(r_name, sites) {
  print(r_name)
  m_name <- gsub(".*duration(.+).tif", "\\1", r_name)

  duration <- gsub(".*duration(.+)_ARI.*", "\\1", r_name)
  ari <- gsub(".*ARI(.+).tif", "\\1", r_name)

  r <- raster(glue("hirdsv4/{r_name}"))
  r <- projectRaster(r, crs = 2193) # reproject to NZTM

  sites$val <- extract(r, sites[c("easting", "northing")], sp = T)
  sites %>%
    mutate(
      hirds = m_name,
      duration = duration,
      ari = ari
    ) %>%
    dplyr::select(c(site, hirds, duration, ari, val))
}

#extract_hirds("hirds_rainfalldepth_duration6.0_ARI100.0.tif", sites)

hirds_files <- list.files(path = "hirdsv4", pattern = "*.tif")
hirds_res <- do.call("rbind", lapply(hirds_files, extract_hirds, sites))

saveRDS(hirds_res, "hirds_tdc_sites.rds")
