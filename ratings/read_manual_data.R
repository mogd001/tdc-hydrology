library(tidyverse)
library(readxl)
library(lubridate)

rating_change_df <- read_excel("data/Rating Change Register.xlsx",
                               sheet = "main") %>% 
  mutate(site = as.factor(site),
         date_review = ymd_hms(paste0(date_review, " 00:00:00"), tz = "NZ"),
         date_new_rating =  ymd_hms(paste0(date_new_rating, " 00:00:00"), tz = "NZ")) %>% 
  drop_na(date_new_rating)

##### - to replace with directly reading from sharepoint once permission has been approved.
# install.packages("Microsoft365R")
# 
# library(Microsoft365R)
# list_sharepoint_sites()
# 
# 
# site <- sharepoint_site("https://tasmandc.sharepoint.com/sites/enviromonitor")
# 
# # Compiled table of all the rating changes/adjustments at each site
# rating_change_df <- read_excel("//tasmandc.sharepoint.com/:x:/r/sites/enviromonitor/_layouts/15/Doc.aspx?sourcedoc=%7B6EBE98D9-31EF-4300-9A0F-3F88A9932D08%7D&file=Rating%20Change%20Register.xlsx",
#            sheet = "main") %>% 
#       mutate(site = as.factor(site),
#              date_review = ymd_hms(paste0(date_review, " 00:00:00"), tz = "NZ"),
#               date_new_rating =  ymd_hms(paste0(date_new_rating, " 00:00:00"), tz = "NZ")) %>% 
#       drop_na(date_new_rating)
#####