library(tdcR)

site <- "HY Waimea at TDC Nursery"
site_information <- read_site_information_from_envmon(site)


hirds_table <- tabulate_hirds_data_string(site_information$hirds_data)
