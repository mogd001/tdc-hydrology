library(tidyverse)
library(lubridate)
library(RODBC)
library(glue)

today <- format(Sys.time(), "%Y%m%d")

get_flood_thresholds <- function(site_name) {
  # Function to read site information to populate rainfall report
  envmon_string <-
    "driver={SQL Server};server=TSRVSQL14;database=ENVMON;trusted_connection=true"
  envmon <- odbcDriverConnect(envmon_string)
  
  flood_thresholds <- sqlQuery(
    envmon,
    paste0(
      "SELECT [SMCode]
      ,[T1Limit]
      ,[T1Colour]
      ,[T1Label]
      ,[T2Limit]
      ,[T2Colour]
      ,[T2Label]
      ,[T3Limit]
      ,[T3Colour]
      ,[T3Label]
      ,[T4Limit]
      ,[T4Colour]
      ,[T4Label]
      ,[T5Limit]
      ,[T5Colour]
      ,[T5Label]
      ,[T6Limit]
      ,[T6Colour]
      ,[T6Label]
      ,[T1Enable]
      ,[T2Enable]
      ,[T3Enable]
      ,[T4Enable]
      ,[T5Enable]
      ,[T6Enable]
      ,[Normal]
      ,[LowerLimit]
      ,[UpperLimit]
      ,[PresetMin]
      ,[PresetMax]
      ,[ThresholdInfo]
      ,[MaxQC]
      FROM [ENVMON].[dbo].[Threshold]",
      " WHERE SMCode Like '%;Flow%'" # OR  SMCode Like '%;Forecast Flow%'"
    ),
    stringsAsFactors = FALSE
  ) 
  # Close db connection
  odbcClose(envmon)
  
  return(flood_thresholds)
}


# tidy up and export
flood_thresholds <- get_flood_thresholds() %>% 
  separate(SMCode, c("site", "threshold_type"), sep = ";")  %>% # Split SMCode in sitename and threshold type
  select(site:T6Enable) %>% 
  rename_all(., .funs = tolower) %>% 
  write_csv(glue("{today}_flow_thresholds.csv"))
