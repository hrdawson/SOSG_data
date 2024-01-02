# Import tree-level data
# Read in files ----
# Make file list
filesTrees <- dir(path = "raw_data/fixed area plot (permanent plot) data", pattern = ".xlsx",
                  full.names = TRUE, recursive = TRUE)

# Read in data
tempTrees = map_dfr(filesCover, read_xlsx, sheet = "Tree-level", skip = 1,
                    col_names = FALSE)

# Clean data ----
# Function to replace binaries with letters
one_to_letter = function(.data, ...) {
  .data |>
  pivot_longer(cols = ...,
               names_to = "temp", values_to = "values") |>
    mutate(values = case_when(
      values == 1 ~ temp,
      TRUE ~ NA
    )) |>
    select(-c(temp)) |>
    group_by(plot) |>
    fill("values", .direction = "downup") |>
    distinct()
}

# Test code for the function
# subset = tree.data |>
#   slice_head(n = 10) |>
#   one_to_letter(c(e:n))
#
# subset = tree.data |>
#   slice_head(n = 10) |>
# pivot_longer(e:n,
#              names_to = "temp", values_to = "value") |>
#   mutate(value = case_when(
#     value == 1 ~ temp,
#     TRUE ~ NA
#   )) |>
#   rename(valuesTo = value) |>
#   select(-temp) |>
#   group_by(plot) |>
#   fill("valuesTo", .direction = "downup") |>
#   distinct()

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
    spp == NA ~ "dud",
    TRUE ~ "okay"
  )) |>
  # Remember that filters drop NAs
  filter(flag == "okay") |>
  # Some data entry was not completed
  # THIS STEP IS DANGEROUS, check here first for errors
  fill(plot, .direction = "down") |>
  # Work with the data that should have one value per plot
  pivot_longer(e:n, names_to = "temp", values_to = "canopy") |>
  mutate(canopy = case_when(
    canopy == 1 ~ temp,
    TRUE ~ NA
  )) |>
  select(-temp) |>
  distinct()
