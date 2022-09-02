library(zoo)
library(tidyverse)
library(lubridate)
library(Hilltop)
library(ggpubr)

source("functions.R")

# -- Setup
hydro_working_dsn <- r"{\\tsrvfiles\hydrology\Datafiles\Hydro-Working.dsn}"
operational_hts <- r"{M:\Datafiles\Operative\OperativeFlow.hts}"

measurement <- "Flow [Water Level]"
start_date = "20220201"
end_date = format(Sys.Date(), format = "%Y%m%d")

# Get sites with flow and create named site choices list
sites <- get_flow_sites(hydro_working_dsn) %>%
  as_tibble() %>% 
  rename_with(tolower) %>%
  filter(!site %in% c("HY Anatoki at Bencarri"))

site_choices <- list()
for (i in seq_along(sites$site)) {
  site_choices[[  sites$site[i] ]] <- i
}
