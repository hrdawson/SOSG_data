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
  drop_na(plot)
  # Convert non-numeric values to numeric
  # For less than 5% cover, replace value with trace amount number

