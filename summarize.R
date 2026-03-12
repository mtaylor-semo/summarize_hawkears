
# Summarize Hawkears ------------------------------------------------------
# 
# For now, just reads in one Audacity labels .txt file, and summarizes
# the number of species detected, min and max confidence for each, and the
# number of detections.

# Future plans could include reading multiple files, sorting/filtering
# results based on a particular confidence level (e.g., < > 0.90),
# joining with a master file of species names to add common names based
# on codes, highlight rarities, or species with fewer than X sightings 
# to quickly highlight recordings to double-check, etc.

library(here)
library(tidyverse)

# Constants
time_zone = "Etc/GMT+6"

# File inputs -------------------------------------------------------------

# Import species codes

spp_codes <- read_delim(
  "data/classes.txt",
  delim = ",",
  col_names = c("species", "sp_code"),
  comment = "#")


# Import data files
files <- fs::dir_ls(path = "data", glob = "*HawkEars.txt")

# Use tmp_data for initial wrangling
tmp_data <- read_tsv(
  files, 
  id = "path",
  col_names = c( # "path" column name added automatically)
    "start_time",
    "end_time",
    "code_confidence"
  ))




# Parse file name ---------------------------------------------------------
#
# Parse imported file names for recorder name, date and time, for possible
# grouping. Remove the "hawkears.txt" suffix.

tmp_data <- tmp_data  |> 
  separate_wider_delim(
    path, 
    delim = "_",
    names = c(
      "recorder",
      "date",
      "file_start_time",
      "delete"
    )) |> 
  select(-delete)

# Delete "data/" from the recorder name to get just the recorder name

tmp_data <- tmp_data |> 
  separate_wider_delim(
    recorder,
    delim = "/",
    names = c(
      "delete",
      "recorder"
    )
  ) |> 
  select(-delete)

# Separate the species code from the confidence value
tmp_data <- tmp_data |> separate_wider_delim(code_confidence,
  names = c("sp_code", "confidence"),
  delim = ";"
)

# Create a POSIX time stamp for each recording based on date and time
# that each new audio file was started. Use "Etc/GMT+6" OlsonCode
# to set consistent time zone independent of daylight savings time.
tmp_data <- tmp_data |> 
  unite(date_time, c("date", "file_start_time"), sep = "") |> 
  mutate(file_start_time = ymd_hms(date_time, tz = time_zone))

# Calculate the relative start time and duration for each detection. This
# should make it easier to find in the audio file when looking at 
# spectrograms.

tmp_data <- tmp_data |>
  mutate(
    detection_start_time = file_start_time + as.period(start_time),
    detection_end_time = file_start_time + as.period(end_time),
    detection_duration = seconds(
      interval(start = detection_start_time, end = detection_end_time)
    ),
    date = as_date(file_start_time),
    recording_time = hms::as_hms(file_start_time),
    detect_date = as_date(file_start_time),
    detect_time = hms::as_hms(detection_start_time)
  )

# hawk_data <- hawk_data |> 
#   mutate(date = as_date(file_start_time),
#          recording_time = hms::as_hms(file_start_time),
#          detect_date = as_date(file_start_time),
#          detec_time = hms::as_hms(detection_start_time))

# Be sure to run sunriset.R before this point.

hawk_data <- left_join(
  x = tmp_data,
  y = sun_df,
  by = "date"
)


# rm(tmp_data)

hawk_data <- hawk_data |>
  mutate(
    rise = hms::as_hms(sunrise),
    set = hms::as_hms(sunset)
  )

hawk_data <- hawk_data |>
  mutate(
    morning_evening = if_else(
      hms::as_hms(file_start_time) < hms::as_hms("12:00:00"),
      "morning", "evening"
    )
  )

hawk1 <- hawk_data |> 
  filter(morning_evening == "morning") |> 
  mutate(
    time_since_sunrise = difftime(
      time1 = detection_start_time,
      time2 = sunrise,
      tz = time_zone,
      units = "mins"),
    time_since_sunset = NA
  )

hawk2 <- hawk_data |> 
  filter(morning_evening == "evening") |> 
  mutate(
    time_since_sunset = difftime(
      time1 = detection_start_time,
      time2 = sunset,
      tz = time_zone,
      units = "mins"),
    time_since_sunrise = NA
  )

hawk_data <- bind_rows(hawk1,hawk2)

rm(hawk1,hawk2)

hawk_data <- hawk_data |>
  mutate(
    time_since_sunrise = difftime(
      time1 = detection_start_time,
      time2 = sunrise,
      tz = time_zone,
      units = "mins")
    )

hawk_summary <- hawk_data |> 
  group_by(recorder, date, sp_code) |> 
  summarise(
    min_conf = min(confidence),
    max_conf = max(confidence),
    N = n(),
    earliest_time = min(time_since_sunrise, na.rm = TRUE),
    latest_time = max(time_since_sunrise, na.rm = TRUE),
    .groups = "keep")

# Add species names to summary file
hawk_summary <- left_join(hawk_summary, spp_codes, by = "sp_code")


hawk_summary |> 
  filter(recorder == "PRAIRIE" & N > 10) |> 
  ggplot() +
  geom_point(aes(x = date,
                 y = log10(N))) +
  facet_wrap(~sp_code) +
  scale_x_date(date_breaks = "weeks")



# Basic Plots -------------------------------------------------------------

# Dumbbell plot for first to last detection. 
# MEH
hawk_summary |>
  ggplot() +
  geom_segment(aes(x = earliest_time, xend = latest_time, y = sp_code)) +
  geom_point(aes(x = earliest_time, y = sp_code)) +
  geom_point(aes(x = latest_time, y = sp_code)) +
  scale_x_time() +
  theme_minimal()

# Scatter plot of detection time by species. So far, this may be the most useful.
hawk_data |> 
  filter(recorder == "PRAIRIE", morning_evening == "morning") |>
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_point(aes(x = time_since_sunrise,
                 y = sp_code,
             shape = recorder,
             color = recorder)) +
  theme_minimal() +
  scale_x_continuous(breaks = c(-60,-30,0,30,60,90,120,150,180))


# Scatter plot of detection time by species. So far, this may be the most useful.
hawk_data |> 
  filter(recorder == "MILLER", morning_evening == "evening") |>
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_point(aes(x = time_since_sunset,
                 y = sp_code,
                 shape = recorder,
                 color = recorder)) +
  theme_minimal() +
  scale_x_continuous(breaks = c(-120,-90,-60,-30,0,30,69))

