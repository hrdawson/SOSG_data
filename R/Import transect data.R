# Import Sabina's transect data
library(tidyverse)
library(tidylog)

source("R/functions/one_to_letter.R")

# Read in files ----
# Make file list
filesTransects <- dir(path = "raw_data/01 - Transect tree-level data", pattern = ".xlsx",
                  full.names = TRUE, recursive = TRUE)

# Read in data
tempTransects = map_dfr(filesTransects, read_xlsx, sheet = "Transect Data", skip = 1,
                    col_names = FALSE)
# Clean data ----
# Need to build the headers separately
transect.data.header = tempTransects |>
  # Select just the headers
  slice_head(n = 2) |>
  # Fill in the NA cells
  fill('...1':'...32', .direction = "down") |>
  # Make just the one row of headers
  slice_tail(n = 1)

transect.data = tempTransects |>
  # Remove the existing header rows
  slice(3:n()) |>
  # Add in correct headers and assign
  add_row(transect.data.header, .before = 1) |>
  row_to_names(row_number = 1) |>
  clean_names(case = "lower_camel") |>
  # Clean out fillers
  mutate(flag = case_when(
    tsct == 1 & tree == 1 & dbh == 21.3 ~ "example",
    tsct == "Tsct" ~ "dud",
    is.na(tsct) ~ "dud",
    TRUE ~ "okay"
  )) |>
  # Remember that filters drop NAs
  filter(flag == "okay") |>
  select(-flag) |>
  # Change numbers to letters
  one_to_letter_transect(c(e:n)) |> rename(canopy = values) |>
  one_to_letter_transect(c(pU:pA)) |> rename(barkDamage = values) |>
  one_to_letter_transect(c(gU:gA)) |> rename(galleries = values) |>
  rename(frass = fh) |>
  one_to_letter_transect(c(u:a)) |> rename(epicormics = values, p = p_2, a = a_2) |>
  one_to_letter_transect(c(h:a)) |> rename(resprouts = values) |>
  # Rename columns not in tree data
  rename(basal.sweep.baf = baf, basal.sweep.count = count,
         transectNr = tsct)

# write.csv(transect.data, "clean_data/transect tree data.csv")

