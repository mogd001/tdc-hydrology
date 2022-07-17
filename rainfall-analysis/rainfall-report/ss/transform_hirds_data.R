hirds_data  <- "1.58,8.77,14.7,19.7,32.3,51.4,99.2,141,188,234,257\r\n2,9.63,16.1,21.6,35.3,56.2,108,154,205,255,280\r\n5,12.6,21,28.2,46,72.9,140,198,264,327,357\r\n10,14.9,24.7,33.2,53.9,85.3,163,231,307,379,414\r\n20,17.2,28.6,38.3,62.2,98.2,187,264,351,433,472\r\n30,18.6,30.9,41.4,67.1,106,202,284,377,465,507\r\n40,19.7,32.6,43.7,70.7,112,212,299,396,488,532\r\n50,20.5,34,45.4,73.6,116,221,310,410,506,551\r\n60,21.2,35.1,46.9,75.9,120,227,320,423,521,567\r\n80,22.3,36.8,49.2,79.6,125,238,334,442,544,592\r\n100,23.1,38.2,51,82.5,130,246,346,457,562,611\r\n"

tabulate_hirds_data_string <- function(data_string) {
  # Function to map HIRDS string saved in ENVMON database to tabular form
  col_names <- c("ARI", "10 min", "20 min", "30 min", "1 hour", "2 hour", "6 hour", "12 hour", "1 day", "2 day", "3 day", "4 day", "5 day")
  
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
    m <- cbind(m, rep(NA, length(m[,1])))
  }
  
  hirds_df <- as_tibble(m)
  names(hirds_df) <- col_names
  
  return(hirds_df)
}

df <- tabulate_hirds_data_string(hirds_data) %>% 
  filter(ARI == 2) %>% 
  select(`10 min`)
