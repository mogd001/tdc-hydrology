library(glue)

src_dir <- "C:/Users/matto/OneDrive - Tasman District Council/Desktop/Working/R/tdc-hydrology"
dst_dir <- "M:/Processing/R_Outputs"

# Clean final outputs folder
unlink(glue("{dst_dir}/*"), recursive = TRUE)
#dir.create(paste0(dst_dir, "/rainfall"))
#dir.create(paste0(dst_dir, "/flow"))

### rainfall sparklines

setwd(src_dir)
setwd("rainfall/sparklines")

source("rainfall_sparklines.R")

file.copy(paste0(getwd(),"/rainfall_outputs"), dst_dir, recursive= TRUE)

### flow sparklines
setwd(src_dir)
setwd("flow/sparklines")

source("flow_sparklines.R")

file.copy(paste0(getwd(),"/flow_outputs"), dst_dir, recursive= TRUE)