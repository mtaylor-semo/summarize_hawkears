
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


# Input multiple files ----------------------------------------------------

files <- fs::dir_ls(path = "data", glob = "*.txt")

hawk_data <- read_tsv(
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

hawk_data <- hawk_data  |> 
  separate_wider_delim(
    path, 
    delim = "_",
    names = c(
      "recorder",
      "date",
      "time",
      "delete"
    )) |> 
  select(-delete)

# Delete "data/" from the recorder name to get just the recorder name

hawk_data <- hawk_data |> 
  separate_wider_delim(
    recorder,
    delim = "/",
    names = c(
      "delete",
      "recorder"
    )
  ) |> 
  select(-delete)


hawk_data <- hawk_data |> separate_wider_delim(code_confidence,
  names = c("sp_code", "confidence"),
  delim = ";"
)




hawk_data |> 
  group_by(sp_code) |> 
  summarise(
    min_conf = min(confidence),
    max_conf = max(confidence),
    N = n())


