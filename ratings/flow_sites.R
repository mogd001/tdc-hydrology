source("functions.R")
hydro_working_dsn <-
  r"{\\tsrvfiles\hydrology\Datafiles\Hydro-Working.dsn}"

# Get flow sites and create named site choices list

sites <- get_flow_sites(hydro_working_dsn) %>%
  as_tibble() %>%
  rename_with(tolower)

site_choices <- list()
for (i in seq_along(sites$site)) {
  site_choices[[sites$site[i]]] <- i
}

print(site_choices)