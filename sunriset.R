library(suncalc)
library(lubridate)
library(tidyverse)

# First attempt at getting sunrise and sunset times. Note that
# 'Etc/GMT+6' is Olson Code for central standard time. The recorders
# will be kept in standard time, never daylight savings time.
fred <- getSunlightTimes(
  date = as.Date("2026-03-11"), 
  lat = 37.132025, 
  lon = -89.461307,
  tz = "Etc/GMT+6", 
  keep = c("sunrise", "sunset"))


# Modified from https://stackoverflow.com/a/68550338/3832941
start.date = "20260201"; end.date = "20260315"
Dates <- seq(ymd(start.date),ymd(end.date), by = "days")
latitude = 37.132025
longitude = -89.461307

# sun_df <- expand.grid(Dates = Dates,County = my_df$County) %>% 
#   left_join(my_df) %>%
#   group_by(Dates, County, Latitude, Longitude) %>% 
#   mutate(sunrise = getSunlightTimes(Dates,latitude,longitude,tz = "Etc/GMT+6")$sunrise,
#          sunset = getSunlightTimes(Dates,latitude,longitude,tz = "Etc/GMT+6")$sunset)  

sun_df <- as_tibble(Dates)

sun_df <- sun_df |> 
  mutate(sunrise = getSunlightTimes(Dates,latitude,longitude,tz = "Etc/GMT+6")$sunrise,
         sunset = getSunlightTimes(Dates,latitude,longitude,tz = "Etc/GMT+6")$sunset)
