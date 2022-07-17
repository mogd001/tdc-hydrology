library(zoo)
library(tidyverse)
library(XML)
library(xml2)
library(httr)
library(lubridate)
library(Hilltop)
library(hillr)
library(ggpubr)
library(plotly)
library(htmlwidgets)
library(scales)
library(forcats)

# https://www.r-bloggers.com/2021/03/detect-the-changes-in-timeseries-data/
library(changepoint)

library(tdcR)
source("date_functions.R")

# -- Setup
endpoint <- "http://envdata.tasman.govt.nz/data.hts?"
hydro_working_dsn <- r"{\\tsrvfiles\hydrology\Datafiles\Hydro-Working.dsn}" # http://envdata.tasman.govt.nz/data.hts?Service=Hilltop&Request=SiteList
operative_hts <- r"{M:\Datafiles\Operative\OperativeFlow.hts}" # http://envdata.tasman.govt.nz/OperativeFlow.hts?Service=Hilltop&Request=SiteList

# read manual change data
source("read_manual_data.R") # returns rating_change_df

# Get flow sites and create named site choices list
sites <- get_flow_sites(hydro_working_dsn) %>%
  as_tibble() %>%
  rename_with(tolower)

site_choices <- list()
for (i in seq_along(sites$site)) {
  site_choices[[sites$site[i]]] <- i
}
site_choices <-
  site_choices[site] # limit sites for initial delivery.

# -- Load ratings and gauging for site
ratings <- load_ratings(endpoint, site, start_date, end_date)
gaugings <- load_gaugings(endpoint, site, start_date, end_date)

flow_working <-
  load_flow(hydro_working_dsn,
            site,
            "Flow [Water Level]",
            start_date,
            end_date)
flow_operative <-
  load_flow(operative_hts, site, "Flow [Flow]", start_date, end_date)

# Construct base df
df <- tibble(datetime = seq(
  as.POSIXct(start_date, format = "%Y%m%d", tz = "NZ"),
  as.POSIXct(end_date, format = "%Y%m%d", tz = "NZ"),
  by = "5 mins"
)) %>%
  mutate(
    hour = hour(datetime),
    weekday = wday(datetime, week_start = 1),
    date = as.Date(datetime, tz = "NZ"),
    year = year(datetime)
  ) %>% # Merge base df with working and operative data and ratings plus gaugings
  left_join(flow_working, by = "datetime") %>%
  rename("flowwork" = "flow") %>%
  left_join(flow_operative, by = "datetime") %>%
  rename("flowoper" = "flow") %>%
  left_join(ratings[, c("start_time", "rating")], by = c("datetime" = "start_time")) %>%
  left_join(gaugings[, c("datetime", "gauging")], by = c("datetime" = "datetime")) %>%
  fill(rating, .direction = "up") %>%
  mutate(
    rating = replace_na(rating, max(ratings$rating) + 1),
    flowdiff = flowoper - flowwork,
    deviation = flowdiff / flowwork * 100  # call the percentage flow difference / flowwork deviation
  ) %>%
  group_by(rating) %>% # replace na's with mean of deviations from that rating
  mutate(deviation = ifelse(is.na(deviation), mean(deviation, na.rm = T), deviation)) %>%
  ungroup()

# Apply change point detection to identify rating adjustments
fit_changepoint = cpt.mean(df$deviation, penalty = "Manual", method = 'BinSeg')
cp_est <- c(ints = param.est(fit_changepoint)$mean, cp = cpts(fit_changepoint))
plot(fit_changepoint, cpt.col = 'blue')

cps <- cp_est[grepl("cp", names(cp_est))]
length(cps)

change_points <- df %>%
  rownames_to_column("rn") %>%
  mutate(
    rn = as.numeric(rn),
    cp = ifelse(rn %in% cps, 1, 0),
    date = as.Date(datetime, tz = "NZ"),
    origin = "rating-adjustment"
  ) %>%
  rename(start_time = datetime) %>%
  filter(cp == 1) %>%
  select(c(start_time, date, origin))

# r_df is then the combination of rating changes and adjustments
r_df <- ratings %>% select(c(start_time, date, origin))

# r_df <- rbind(r_df, change_points) %>%
#   arrange(start_time) %>%
#   mutate(prev_st = lag(start_time),
#          diff = start_time - prev_st) %>%
#   filter(abs(diff) > hours(24) |
#            is.na(prev_st) | origin == "rating-change") %>%
#   select(c(start_time, date, origin)) %>%
#   arrange(desc(start_time)) %>%
#   mutate(prev_st = lag(start_time),
#          diff = start_time - prev_st) %>%
#   filter(abs(diff) > hours(24) |
#            is.na(prev_st) | origin == "rating-change") %>%
#   arrange(start_time) %>%
#   mutate(origin = fct_relevel(origin, "rating-change", "rating-adjustment"))

site_filter <- site

r_df <- rating_change_df %>%
  filter(site == site_filter) %>%
  transmute(start_time = date_review,
         date = as.Date(start_time),
         origin = "rating-adjustment") %>%
  distinct() %>%
  rbind(r_df) %>%
  arrange(start_time) %>%
  mutate(origin = fct_relevel(origin, "rating-change", "rating-adjustment"))

df <- df %>%
  left_join(filter(r_df, origin == "rating-adjustment")[, c("start_time", "origin")],
            by = c("datetime" = "start_time")) %>%
  rename(adjustment = origin)

# summary table
summary_table <- df %>%
  group_by(rating) %>%
  summarise(
    start = min(datetime),
    end = max(datetime),
    mid_point = as.POSIXct(paste0(mid_date(as.Date(start), as.Date(end)), " 00:00:00"), tz = "NZ"),
    t_rating = round(max(datetime) - min(datetime), 1),
    
    t_last_gauging_to_rating_change = round(max(datetime) - max(datetime[!is.na(gauging)]), 1),
    
    n_gaugings = sum(!is.na(gauging)),
    n_adjustments = sum(!is.na(adjustment)),
    
    lower_15th = round(quantile(deviation, 0.15, na.rm = TRUE), 1),
    median = round(median(deviation, na.rm = TRUE), 1),
    mean = round(mean(deviation, na.rm = TRUE), 1),
    upper_85th = round(quantile(deviation, 0.85, na.rm = TRUE), 1),
    sd = round(sd(deviation, na.rm = TRUE), 1)
  )

summary_table[1, "start"] <- NA
summary_table[1, "t_rating"] <- NA
summary_table[nrow(summary_table), "end"] <- NA
summary_table[nrow(summary_table), "t_last_gauging_to_rating_change"] <- NA

# -- Visualise
theme_set(theme_bw())


max_flow = max(df$flowwork, na.rm = TRUE)
annotation <- data.frame(
  x = summary_table$mid_point,
  y = rep(c(max_flow * 1.5),each=length(summary_table$rating)),
  label = paste0("R. ", summary_table$rating)
)

g1 <- ggplot(data = df) +
  geom_line(aes(x = datetime, y = flowoper, color = "Operative")) +
  geom_line(aes(x = datetime, y = flowwork, color = "Working")) +
  geom_point(data = gaugings, aes(datetime, max_flow * 0.9), shape = 8, size = 4, color = "red") +
  geom_vline(
    xintercept = ratings$start_time,
    alpha = 0.5,
    lty = 2,
    color = "red"
  ) + 
  geom_text(data = annotation, aes(x = x, y = y, label = label),
             color = "red",
             size = 4, fontface = "bold") +
  coord_cartesian(
    xlim = c(min(df$datetime),
             max(df$datetime)),
    ylim = c(0.1, max_flow * 2.0),
    expand = FALSE
  ) +
  scale_x_datetime(breaks = seq(min(df$datetime), max(df$datetime), by = "1 months"),
                   date_labels = "%b-%y") +
  scale_y_log10() +
  ggtitle(paste0(site, " Flow Plot"),
          subtitle = "Points represent gaugings, note log scale on the vertical axis.") +
  labs(x = "", y = "Flow (m3/s)", color = "Flow") +
  scale_color_manual(values = c("green", "blue")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))

g2 <- ggplot() +
  geom_line(data = df,
            aes(x = datetime, y = deviation),
            color = "black") +
  geom_point(
    data = r_df,
    aes(x = start_time, y = 0, color = origin),
    size = 5,
    shape = 2
  ) +
  coord_cartesian(xlim = c(min(df$datetime),
                           max(df$datetime)),
                  expand = FALSE) +
  scale_x_datetime(breaks = seq(min(df$datetime), max(df$datetime), by = "1 months"),
                   date_labels = "%b-%y") +
  geom_vline(
    xintercept = ratings$start_time,
    alpha = 0.5,
    lty = 2,
    color = "red"
  ) +
  ggtitle(paste0(site, " Deviation Plot - Operative over Working")) +
  labs(x = "Datetime (NZST)", y = "Deviation (%)", color = "") +
  scale_color_manual(values = c("red", "orange")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))


g3 <- ggplot(data = df, aes(x = as.factor(rating), y = deviation)) +
  geom_boxplot() +
  ggtitle(paste0(site, " Deviation Plot - by Rating")) +
  labs(x = "Rating", y = "Deviation (%)", color = "")
  
g4 <- ggplot(data = df, aes(deviation)) +
  geom_density(alpha = 0.3, fill = "Blue") + 
  facet_wrap(~rating, ncol = length(unique(df$rating)), scale = "free") +
  labs(x = "Deviation (%)", y = "Density", fill = "") +
  coord_flip(xlim = c(-20,20))

#saveWidget(ggplotly(g2), file=paste0("outputs/", site, ".html"))
g <- ggarrange(
  g1,
  g2,
  g3,
  g4,
  heights = c(2, 2),
  ncol = 1,
  nrow = 4,
  align = "v"
)

png(
  file = paste0("outputs/", site, ".png"),
  width = 900,
  height = 2000
)
g
dev.off()