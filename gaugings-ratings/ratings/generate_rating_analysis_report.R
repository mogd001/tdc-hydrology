library(rmarkdown)

#site <- "HY Takaka at Kotinga"

sites <- get_flow_sites(hydro_working_dsn) %>%
  as_tibble() %>%
  rename_with(tolower) %>% 
  filter(site %in% c("HY Motueka at Gorge", "HY Anatoki at Happy Sams",
                      "HY Aorere at Devils Boots", "HY Motueka at Woodmans Bend",
                      "HY Wangapeka at Walter Peak", "HY Tadmor at Mudstone",
                      "HY Motupiko at Christies", "HY Waimea at TDC Nursery"))

#site <- "HY Motueka at Gorge" # operative flow start 20191001
#site <- "HY Waingaro at Hanging Rock"
start_date <- "20200101"
# end_date <- "20220101"
end_date <- format(Sys.Date(), format = "%Y%m%d")

no_report_sites <- c()

for (site in sites$site) {
  print(site)
  
  tryCatch({
    rmarkdown::render(
      "rating_analysis_output.Rmd",
      output_file = paste(
        "outputs/rating-analysis.",
        Sys.Date(),
        ".",
        site,
        ".html",
        sep = ""
      )
    )
  },
  error = function(err) {
    print(site)
    no_report_sites <- append(no_report_sites, site)
  })
}

print(no_report_sites)