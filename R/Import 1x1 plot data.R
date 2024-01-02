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
cover.data = tempCover |>
  # Select just the cover columns + meta columns
  select('...1':'...5', '...27':'...37') |>
  # Remove the existing header rows
  slice(4:n()) |>
  # Add in correct headers and assign
  add_row(cover.data.header, .before = 1) |>
  row_to_names(row_number = 1) |>
  clean_names(case = "lower_camel") |>
  # Clean the data
  # Some data entry was not completed
  # THIS STEP IS DANGEROUS, check here first for errors
  fill(plot, .direction = "down") |>
  # Now we can group by plot
  group_by(plot) |>
  fill(assessor1:date, .direction = "down") |>
  ungroup() |>
  # Filter out extraneous row names
  filter(plot != "Plot") |>
  # Pivot longer so we can work with all at once
  pivot_longer(cols = mallee:bareSoil, names_to = "typeCover", values_to = "percent") |>
  # Replace non-numeric values
  mutate(percent = case_when(
    percent == "<5" ~ "0.1",
    percent == "<10" ~ "0.1",
    percent == ">5" ~ "0.1",
    TRUE ~ percent
  ),
  percent = as.numeric(percent),
         percent = replace_na(percent, 0),
         # Convert date to human readable
         date = openxlsx::convertToDate(date))

# write.csv(cover.data, "clean_data/1x1 cover data.csv")
