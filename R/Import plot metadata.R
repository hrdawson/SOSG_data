# Read in files ----
# Make file list
filesMeta <- dir(path = "raw_data/fixed area plot (permanent plot) data", pattern = ".xlsx",
                    full.names = TRUE, recursive = TRUE)

# Read in data
tempMeta = map_dfr(filesSubplot, read_xlsx, sheet = "Plot-level")

# Clean data ----
## Make the row header
meta.data.header = tempMeta |>
  # Select just the headers
  slice_head(n = 2) |>
  # Fill in the NA cells
  fill('ADD PLOT-LEVEL BEFORE ADDING TREE-LEVEL ATTRIBUTES':'...27', .direction = "down") |>
  # Make just the one row of headers
  slice_tail(n = 1)

meta.data = tempMeta |>
  # Remove the existing header rows
  slice(3:n()) |>
  # Add in correct headers and assign
  add_row(meta.data.header, .before = 1) |>
  row_to_names(row_number = 1) |>
  clean_names(case = "lower_camel") |>
  rename(latitudeStart = latitude, longitudeStart = longitude,
         latitudeEnd = latitude_2, longitudeEnd = longitude_2,
         elevationStart = elevation, elevationEnd = elevation_2) |>
  # Clean out extra bits
  drop_na(plot) |>
  filter(plot != "Plot") |>
  filter(assessor1 != "MTB") |>
  # Convert binaries to letters
  # BEWARE: ALL `1` VALUES WILL BE REPLACED
  # This code alters the plot numbers as well, so temporarily stash them in rownames
  column_to_rownames(var = "plot") |>
  # It also messes with the waypoint and photopoint column, so dropping them to avoid confusion
  select(-c(wp, pp)) |>
  # Code from https://stackoverflow.com/questions/50138295/replace-column-values-with-column-name-using-dplyrs-transmute-all
  imap_dfr(~replace(.x, .x==1, .y)) |>
  # Bring back plot
  rownames_to_column(var = "plot") |>
  # Pivot to make this sensible
  pivot_longer(c:f, names_to = "temp", values_to = "landform") |>
  drop_na(landform) |>
  select(-temp) |>
  pivot_longer(x:i, names_to = "temp", values_to = "inclination") |>
  select(-temp) |>
  distinct() |>
  # Work around to deal with inability to drop NAs
  # (Dropping them removes useful landform data)
  group_by(plot) |>
  fill(inclination, .direction = "downup") |>
  ungroup() |>
  distinct()
