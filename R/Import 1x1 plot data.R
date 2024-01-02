# Read in files ----
# Make file list
filesCover <- dir(path = "raw_data/fixed area plot (permanent plot) data", pattern = ".xlsx",
                    full.names = TRUE, recursive = TRUE)

# Read in data
tempCover = map_dfr(filesCover, read_xlsx, sheet = "1 x 1 subplots", skip = 1,
                      col_names = FALSE)


# First, build a way to read in one of these sheets before tackling all of them

# Cover data ----
# Need to build the headers separately
cover.data.header = tempCover |>
  # Select just the cover columns + meta columns
  select('...1':'...5', '...27':'...37') |>
  # Select just the headers
  slice_head(n = 3) |>
  # Fill in the NA cells
  fill('...1':'...37', .direction = "down") |>
  # Make just the one row of headers
  slice_tail(n = 1)

# Import the data
cover.data = read_excel("raw_data/fixed area plot (permanent plot) data/Plot 01.xlsx",
                               sheet = "1 x 1 subplots", skip = 1,
                               #Going to get messy to select just the columns I need
                               col_names = FALSE) |>
  # Select just the cover columns + meta columns
  select('...1':'...5', '...27':'...37') |>
  # Remove the existing header rows
  slice(4:n()) |>
  # Add in correct headers and assign
  add_row(cover.data.header, .before = 1) |>
  row_to_names(row_number = 1) |>
  clean_names(case = "lower_camel") |>
  # Clean the data
  # Pivot longer so we can work with all at once
  pivot_longer(cols = mallee:bareSoil, names_to = "typeCover", values_to = "percent") |>
  # Replace non-numeric values
  mutate(percent = as.numeric(str_replace(percent, "<5", "0.1")),
         percent = replace_na(percent, 0),
         # Convert date to human readable
         date = openxlsx::convertToDate(date))
