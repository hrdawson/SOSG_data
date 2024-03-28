# Export dendros and GPS points ----
# These objects are from `Canopy health classes.R`
gps.extra = read.csv("raw_data/SOSG dendro GPS points added on.csv") |> rename(GPS2 = GPS) |>
  select(dendroNr, GPS2)

tree.data.gps = tree.data.all.canopy |>
  select(plotNickname, plot, tree, stem, dbh, GPS, dendroNr, dendroType) |>
  # Resolve funny formatting of dendroNr
  mutate(dendroNr = case_when(
    dendroNr == "15?" ~ 15,
    TRUE ~ dendroNr),
    dendroNr = round(as.numeric(dendroNr), 1),
    GPS = as.integer(GPS)) |>
  # Select trees with dendros
  drop_na(dendroNr) |>
  filter(dendroType != "none") |>
  distinct() |>
  # Add in missing points
  left_join(gps.extra)  |>
  # Add in plot nicknames
  mutate(plotNickname = case_when(
    plot == 7.2 ~ "Spencers Creek",
    plot == 21 ~ "Illawong",
    plot == 22 ~ "Pipers Gap",
    TRUE ~ plotNickname
  )) |>
  # Bring together the two GPS columns
  mutate(GPSpoint = coalesce(GPS, GPS2)) |>
  arrange(dendroNr)

# write.csv(tree.data.gps, "outputs/2024.03.28_dendroGPSPoints.csv")

# Find missing dendros ----
## From the R data ----
test = tree.data.all.canopy |>
  select(plotNickname, plot, tree, stem, dbh, GPS, dendroNr, dendroType) |>
  # Select trees with dendros
  drop_na(dendroNr) |>
  filter(dendroType != "none") |>
  distinct()

missing.dendro = gps.extra |>
  anti_join(test)

## From the GIS data ----
dendro.check = as.data.frame(c(1:172, 201:306)) |> #Tags excluded have not been put on trees
  rename(dendroNr = 'c(1:172, 201:306)') |>
  anti_join(read.csv("raw_data/Dendrometers_from_spatial.csv"))
