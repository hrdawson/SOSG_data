# 5x5 Plot data manager

# First, build a way to read in one of these sheets before tackling all of them
library(readxl)
library(janitor)
library(tidyverse)
library(tidylog)

data = read_excel("raw_data/fixed area plot (permanent plot) data/Plot 01.xlsx",
                  sheet = "5 x 5 subplots", skip = 1,
                  range = cell_cols("A:J")) |>
  row_to_names(row_number = 1) |>
  clean_names(case = "lower_camel") |>
  drop_na(plot) |>
  # Convert non-numeric values to numeric
  # For less than 5% cover, replace value with trace amount number
  mutate(coverPercent = str_replace(coverPercent, "<5", "0.1"),
         # Abundance is trickier because it's a large size class
         # IE <1000 means between 500 and 1000
         # But running on the idea of relative proportion
         abundance = case_when(
           abundance == "<100" ~ "100",
           abundance == "<1000" ~ "999",
           abundance == ">1000" ~ "1000",
           abundance == "<20" ~ "20",
           abundance == "<5" ~ "5",
           abundance == "<50" ~ "50",
           abundance == "<500" ~ "500",
           TRUE ~ abundance
         )) |>
  # Convert relevant columns to numeric
  mutate_at(c("midlineDistance", "sample", "coverPercent", "abundance",
              "cwdLengthM"), as.numeric)
