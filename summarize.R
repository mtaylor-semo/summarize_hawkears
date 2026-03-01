
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

hawk <- read_table(
  "data/MILLER_20260214_054800_HawkEars.txt",
  col_names = c("start_time", "end_time", "code_confidence")
)

hawk <- hawk |> separate_wider_delim(code_confidence,
  names = c("sp_code", "confidence"),
  delim = ";"
)


hawk |> 
  group_by(sp_code) |> 
  summarise(
    min_conf = min(confidence),
    max_conf = max(confidence),
    N = n())


hawk |>   count(sp_code)

hawk
