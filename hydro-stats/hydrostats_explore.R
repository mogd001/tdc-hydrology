install.packages("hydrostats")

library(hydrostats)
source("../my-utils/get_data_hilltopserver.R")

?hydrostats
help(hydrostats)

# get flow data
start_time <- Sys.time()
df <- get_data_hilltop_server(collection = "ActiveFlowSites", method = "Average", time_interval = NULL, from = "2019-01-01", to = "2021-12-31", interval = "1 day", alignment = "00:00") %>% 
  rename(flow = value)
end_time <- Sys.time()
end_time - start_time

saveRDS(df, file = "flow.rds")
df <- readRDS(file = "flow.rds")

####
ggplot(df, aes(datetime, flow)) +
  geom_line() +
  facet_wrap(~site, scales = "free_y") +
  theme_bw()
####

anatoki_happy_sams_df <- df %>% 
  filter(site == "HY Anatoki at Happy Sams" & !is.na(flow)) %>% 
  mutate(Date = datetime,
         Q = flow * 86.4) # convert to ML/day, 86.4

# exploring hydrostats
?partial.series
plot(anatoki_happy_sams_df$Date, anatoki_happy_sams_df$Q, type="l", xlab="Date", ylab="Discharge (ML/day)")

ann.cv(anatoki_happy_sams_df)
baseflows(anatoki_happy_sams_df, a = 0.975, n.reflected = 30, ts = "mean")

baseflows(anatoki_happy_sams_df,a=0.975, ts="mean")
baseflows(anatoki_happy_sams_df,a=0.975, ts="annual")
head(baseflows(anatoki_happy_sams_df,a=0.975, ts="daily"))

daily.cv(anatoki_happy_sams_df) # (sd/mean)*100

flood.length.max(anatoki_happy_sams_df, threshold = 1500, ind.days = 5)

high.spells(anatoki_happy_sams_df)
high.spell.lengths(anatoki_happy_sams_df)

low(anatoki_happy_sams_df)
low.spell.lengths(anatoki_happy_sams_df)

partial.series(anatoki_happy_sams_df, plot = TRUE)

# test subset of data (2019-03-25 -> 2019-04-05)
sub <- anatoki_happy_sams_df %>% filter(Date > ymd("2021-01-01") & Date <  ymd("2021-12-31"))

ggplot(sub, aes(Date, Q)) +
  geom_line() + 
  theme_bw() +
  labs(x = "Date", y = "Flow (ML/day)")

low.spell.lengths(sub)
high.spell.lengths(sub)
high.spells(sub)
