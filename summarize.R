
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

# Import species codes

spp_codes <- read_delim(
  "data/classes.txt",
  delim = ",",
  col_names = c("species", "sp_code"),
  comment = "#")




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


rm(tmp_data)

hawk_data <- hawk_data |> 
  mutate(rise = hms::as_hms(sunrise),
         set = hms::as_hms(sunset))

hawk_data <- hawk_data |>
  mutate(
    time_since_sunrise = difftime(
      time1 = detection_start_time,
      time2 = sunrise,
      tz = time_zone,
      units = "mins")
    )

hawk_summary <- hawk_data |> 
  group_by(recorder, sp_code) |> 
  summarise(
    min_conf = min(confidence),
    max_conf = max(confidence),
    N = n(),
    .groups = "keep")

# Add species names to summary file
hawk_summary <- left_join(hawk_summary, spp_codes, by = "sp_code")





