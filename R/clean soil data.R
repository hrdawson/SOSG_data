# Soils data cleaning
library(tidyverse)
library(readxl)
library(janitor)
library(tidylog)

SC.data = read_excel("raw_data/Brookhouse 2023 Project Spreadsheet Soil analyses.xlsx",
                     skip = 1) |>
  # Dates and times will need fixing, but that's a problem for another day
  clean_names("upper_camel", replace=janitor:::mu_to_u) |>
  # Make sure NAs are read in
  mutate(across(where(is.character), ~na_if(., "n/a"))) |>
  # Filter to just soil chemical analyses
  filter(SampleType == "Soil") |>
  # Remove columns with no data (from https://community.rstudio.com/t/drop-all-na-columns-from-a-dataframe/5844/2)
  map(~.x) %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x)

