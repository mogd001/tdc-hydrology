library(tidyverse)
library(lubridate)
library(readr)
library(Hilltop)

################################################################################
#
# Title: Rainfall Analysis
# Author: Matt Ogden
# Description: Load some rainfall data and produce some simple summaries.
#
################################################################################

# Load data ---------------------------

# Create site list and measurement list.
sites <- NULL
measurements <- NULL

sites$site <-
  c("HY Anatoki at Happy Sams", "HY Richmond Weather at TDC Roof")

measurements$measurements < -c("Rainfall")

# Set the time range for the requests
startDate <- "1/1/2020"
endDate <- "1/1/2022"

tss_url <- "http://envdata.tasman.govt.nz/data.hts?"

ls <- length(sites$site)
lm <- length(measurements$measurements)

output <- NULL

for (s in sites$site) {
  # start the loop for sites
  toutput <- NULL #create an empty dataframe for the temporary output (within the loop)
  
  for (m in measurements$measurements) {
    #start of the loop for measurements
    
    message(paste("Requesting", s, m))
    #build the request
    request <-
      paste(
        "service=Hilltop&request=GetData&Site=",
        site,
        "&Measurement=",
        measurement,
        "&From=",
        startDate,
        sep = ""
      )
    #get the xml data from the server
    url <- paste(tss_url, request, sep = "")
    dataxml <- xmlParse(url)
    #convert the xml into a dataframe of measurement results
    #with basic error handling
    data <- tryCatch({
      hilltopMeasurement(dataxml)
    }, error = function(err) {
      message(paste("Error retrieving", site, measurement))
    })
    
    toutput <-
      rbind(toutput, data) #append the data to the dataframe called toutput (temporary dataframe)
  }
  output <-
    rbind(output, toutput) #append the data to the dataframe called output
}

saveRDS(output, file = "data/temp.rds")
output <- readRDS(file = "data/temp.rds")

# Transformations --------------------------
output[c("Time", "Value", "Site", "Measurement")]

colnames(output)
str(output)

df <- output %>%
  mutate(
    Time = as.POSIXct(output$Time, format = "%Y-%m-%d %H:%M:%S"),
    Value = as.double(Value),
    Rainfall = Value
  ) %>%
  subset(select = -Value)

df_anatoki <- subset(df, Site == "HY Anatoki at Happy Sams")
df_richmond_roof <-
  subset(df, site == "HY Richmond Weather at TDC Roof")

# Calculate Statistics ---------------------------
summary(df_anatoki$Rainfall)
summary(df_richmond_roof$Rainfall)

# A simple way of generating summary statistics by grouping variable is available in the psych packag
library(psych)
describeBy(df, "Site")

library(pastecs)
stat.desc(df_richmond_roof$Rainfall)

# Visualisatons ---------------------------
plot(
  df_anatoki$Time,
  df_anatoki$Rainfall,
  type = "l",
  xlab = "",
  ylab = "Rainfall (mm)",
  sub = "HY Anatoki at Happy Sams Rainfall"
)

# regroup by year using the aggregate function and plot
dates = format(df_anatoki$Time, "%Y")
annual_df <- aggregate(df_anatoki$Rainfall, by = list(dates), FUN = sum)
names(annual_df) = c("Year", "Rainfall")
annual_df$Year = as.factor(annual_df$Year)

library(ggplot2)
ggplot(annual_df, aes(x = Year, y = Rainfall))

ggplot(annual_df, aes(x = Year, y = Rainfall)) +
  geom_bar(stat = "identity")