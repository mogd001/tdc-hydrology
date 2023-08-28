.libPaths(c( "C:/Apps/Hydro/library"))  #.libPaths()
library(glue)
print(glue("lib path: {.libPaths()[1]}"))

library(lubridate)

src_dir <- "C:\\Apps\\Hydro\\tdc-hydrology"
dst_dir <- "\\\\tsrvfiles\\HYDROLOGY\\Processing\\R_Outputs"

# Clean final outputs folder
unlink(glue("{dst_dir}/*"), recursive = TRUE)

## rainfall past 24 hours

setwd(src_dir)
setwd("rainfall/rainfall-summary-p24hrs")

source("rainfall_summary_p24hrs.R")

fs <- list.files(paste0(getwd(),"/outputs"))
file.copy(paste0(getwd(),"/outputs/", fs), dst_dir, recursive = TRUE)

d <- now()
# Archive
arch_dirname <- format(d, "%Y%m%d-%H")

### rainfall sparklines

setwd(src_dir)
setwd("rainfall/sparklines")

source("rainfall_sparklines.R")

fs <- list.files(paste0(getwd(),"/rainfall-sparklines"))
file.copy(paste0(getwd(),"/rainfall-sparklines/", fs), dst_dir, recursive = TRUE)

### flow sparklines

setwd(src_dir)
setwd("flow/sparklines")

source("flow_sparklines.R")

fs <- list.files(paste0(getwd(),"/flow-sparklines"))
file.copy(paste0(getwd(),"/flow-sparklines/", fs), dst_dir, recursive = TRUE)

## rainfall past 7 days

setwd(src_dir)
setwd("rainfall/rainfall-summary-p7days")

source("rainfall_summary_p7days.R")

fs <- list.files(paste0(getwd(),"/outputs"))
file.copy(paste0(getwd(),"/outputs/", fs), dst_dir, recursive = TRUE)
