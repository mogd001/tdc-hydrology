round_any <- function(x, accuracy, f = ceiling) {
  # Function for rounding and setting y limits on plots
  f(x / accuracy) * accuracy
}


read_site_information <- function(site_name) {
  # Function to read site information to populate rainfall report
  envmon_string <-
    "driver={SQL Server};server=TSRVSQL14;database=ENVMON;trusted_connection=true"
  envmon <- odbcDriverConnect(envmon_string)
  site_name_wrapped <- paste0("\'", site_name, "\'")
  site_information <- sqlQuery(
    envmon,
    paste0(
      "SELECT SiteID, Name, Easting, Northing, AuxName1, AuxName2, CatchmentArea, HIRDS
     FROM Site",
      " WHERE Name LIKE ",
      site_name_wrapped
    ),
    stringsAsFactors = FALSE
  ) %>%
    rename(
      site_id = SiteID,
      site_name = Name,
      easting = Easting,
      northing = Northing,
      first_synonym = AuxName1,
      second_synonym = AuxName2,
      catchment_area = CatchmentArea,
      hirds_data = HIRDS
    )
  # Close db connection
  odbcClose(envmon)
  
  return(site_information)
}


tabulate_hirds_data_string <- function(data_string) {
  # Function to map HIRDS string saved in ENVMON database to tabular form
  col_names <-
    c(
      "ARI",
      "10 min",
      "20 min",
      "30 min",
      "1 hour",
      "2 hour",
      "6 hour",
      "12 hour",
      "1 day",
      "2 day",
      "3 day",
      "4 day",
      "5 day"
    )
  
  # Split string according to \r\n rows, then convert rows to double and create dataframe
  rows <- strsplit(data_string, "\r\n")[[1]]
  
  n_col <- 0
  for (row in rows) {
    x <- as.double(unlist(strsplit(row, ",")))
    if (n_col == 0) {
      n_col <- length(x)
      v <- x
    } else {
      v <- append(v, x)
    }
  }
  m <- t(matrix(v, ncol = n_col))
  
  # add empty columns to m
  for (i in 1:(length(col_names) - n_col)) {
    m <- cbind(m, rep(NA, length(m[, 1])))
  }
  
  hirds_df <- as_tibble(m)
  names(hirds_df) <- col_names
  
  return(hirds_df)
}
