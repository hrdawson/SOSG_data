# Import tree-level data
library(tidyverse)
library(tidylog)

source("R/functions/one_to_letter.R")

# Read in files ----
# Make file list
filesTrees <- dir(path = "raw_data/fixed area plot (permanent plot) data", pattern = ".xlsx",
                  full.names = TRUE, recursive = TRUE)

# Read in data
tempTrees = map_dfr(filesTrees, read_xlsx, sheet = "Tree-level", skip = 1,
                    col_names = FALSE)

# Clean data ----
# Need to build the headers separately
tree.data.header = tempTrees |>
  # Select just the headers
  slice_head(n = 2) |>
  # Fill in the NA cells
  fill('...1':'...37', .direction = "down") |>
  # Make just the one row of headers
  slice_tail(n = 1)

tree.data = tempTrees |>
  # Remove the existing header rows
  slice(3:n()) |>
  # Add in correct headers and assign
  add_row(tree.data.header, .before = 1) |>
  row_to_names(row_number = 1) |>
  clean_names(case = "lower_camel") |>
  # Clean out fillers
  mutate(flag = case_when(
    plot == 1 & tree == 1 & dbh == 21.3 ~ "example",
    plot == 1 & tree == 2 & dbh == 110.2 ~ "example",
    plot == "Plot" ~ "dud",
    pres == "Pres" ~ "dud",
    is.na(spp) ~ "dud",
    TRUE ~ "okay"
  )) |>
  # Remember that filters drop NAs
  filter(flag == "okay") |>
  # Some data entry was not completed
  # THIS STEP IS DANGEROUS, check here first for errors
  fill(plot, .direction = "down") |>
  mutate_at(c("plot", "tree", "stem"), as.numeric) |>
  distinct() |>
  # Change numbers to letters
  one_to_letter_stem(c(e:n)) |> rename(canopy = values) |>
  one_to_letter_stem(c(pU:pA)) |> rename(barkDamage = values) |>
  one_to_letter_stem(c(gU:gA)) |> rename(galleries = values) |>
  one_to_letter_stem(c(dB:dN)) |> rename(stem.dead = values) |>
  rename(frass = fh) |>
  one_to_letter_stem(c(u:a)) |> rename(epicormics = values, p = p_2, a = a_2) |>
  one_to_letter_stem(c(h:a)) |> rename(resprouts = values)

# write.csv(tree.data, "clean_data/Tree data.csv")
