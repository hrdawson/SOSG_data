# Subspecies distribution

ALA.ssp = read.csv("spatial/ALA/records-2024-10-23/records-2024-10-23.csv", na.strings = c("", NA)) |>
  # Remove iNat obs
  filter(!basisOfRecord %in% c("OBSERVATION", "OCCURENCE", "HUMAN_OBSERVATION", "LIVING_SPECIMEN")) |>
  # Remove highly uncertain obs
  filter(coordinateUncertaintyInMeters < 5000) |>
  # Remove outlier
  filter(recordID != "47f2035b-43e1-4e80-8959-8d61142f9808") |>
  # Duplicate lat lon so that we can reuse them later
  mutate(lat = decimalLatitude, lon = decimalLongitude) |>
  # Remove missing subspecies
  filter(taxonRank == "subspecies") |>
  # Remove empty cols
  janitor::remove_empty("cols") |>
  # Make lacrimans a subspecies
  mutate(infraspecificEpithet = case_when(
    scientificName == "Eucalyptus lacrimans" ~ "lacrimans",
    scientificName == "Eucalyptus pauciflora subsp. acerina" ~ "acerina",
    scientificName == "Eucalyptus pauciflora subsp. debeuzevillei" ~ "debeuzevillei",
    scientificName == "Eucalyptus pauciflora subsp. hedraia" ~ "hedraia",
    scientificName == "Eucalyptus pauciflora subsp. niphophila" ~ "niphophila",
    scientificName == "Eucalyptus pauciflora subsp. parvifructa" ~ "parvifructa",
    scientificName == "Eucalyptus pauciflora subsp. pauciflora" ~ "pauciflora",
    TRUE ~ infraspecificEpithet),
    specificEpithet = case_when(
      scientificName == "Eucalyptus lacrimans" ~ "lacrimans",
      scientificName %in% c("Eucalyptus pauciflora subsp. acerina",
                            "Eucalyptus pauciflora subsp. debeuzevillei",
                            "Eucalyptus pauciflora subsp. hedraia",
                            "Eucalyptus pauciflora subsp. niphophila",
                            "Eucalyptus pauciflora subsp. parvifructa",
                            "Eucalyptus pauciflora subsp. pauciflora") ~ "pauciflora",
      TRUE ~ specificEpithet)) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Inspect names
ALA.ssp.names = ALA.ssp |>
  select(raw_scientificName, scientificName, genus, specificEpithet, infraspecificEpithet)

ALA.ssp.NA = ALA.ssp |>
  filter(is.na(specificEpithet))

# Check visually ----
mapview(ALA.ssp, zcol = "infraspecificEpithet")

# Extract elevation ---
files <- dir("spatial/worldclim", pattern = "*.tif", full.names = TRUE)
## Combine rasters into a rasterStack
elev.rast = terra::rast(files)

##extract raster data for each point
ALA.ssp.elev = alldata %>%
    terra::extract(ALA.ssp) |>
    select(-ID) |>
    rename(elev.m = wc2.1_30s_elev) |>
  data.frame() |>
  dplyr::bind_cols(ALA.ssp) |>
  # Need to find the records that are likely in the wrong location based on elevation
  mutate(minimumElevationInMeters = coalesce(minimumElevationInMeters, maximumElevationInMeters),
         maximumElevationInMeters = coalesce(maximumElevationInMeters, minimumElevationInMeters),
    elevFlag = case_when(
    elev.m < minimumElevationInMeters - 200 ~ "too low",
    elev.m > maximumElevationInMeters + 200 ~ "too high"
  )) |>
  relocate(minimumElevationInMeters, maximumElevationInMeters, elevFlag, .after = elev.m) |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4326)

## visualise
ggplot(ALA.ssp.elev |> filter(is.na(elevFlag)) |>
         mutate(infraspecificEpithet = factor(infraspecificEpithet, levels = c("pauciflora", "parvifructa", "acerina",
                                                                               "hedraia", "debeuzevillei", "niphophila"))),
       aes(y = elev.m, x = infraspecificEpithet,
                         colour = infraspecificEpithet, fill = infraspecificEpithet)) +
  geom_violin() +
  geom_boxplot(alpha = 0, colour = "grey80", linewidth = 1.5) +
  labs(x = "Subspecies", y = "Elevation (m)") +
  scale_colour_viridis_d(direction = -1) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 24))

ggsave("outputs/2024.10.23_snowGumsByElevation.png", width = 6, height = 10, units = "in")

mapview(ALA.ssp.elev |>
          filter(is.na(elevFlag)) |>
          drop_na(elev.m) |>
          mutate(infraspecificEpithet =
                   factor(infraspecificEpithet,
                          levels = c("niphophila", "debeuzevillei", "hedraia",
                                     "acerina", "parvifructa", "pauciflora"))),
        zcol = "infraspecificEpithet")

# Without subspecies
mapview(ALA.ssp.elev |>
          filter(is.na(elevFlag)) |>
          drop_na(elev.m) |>
          mutate(infraspecificEpithet =
                   factor(infraspecificEpithet,
                          levels = c("niphophila", "debeuzevillei", "hedraia",
                                     "acerina", "parvifructa", "pauciflora"))),
        zcol = "elev.m")

# Whats up with the high elev pauciflora?
ALA.ssp.pauciflora = ALA.ssp.elev |>
  filter(infraspecificEpithet == "pauciflora") |>
  relocate(raw_scientificName, scientificName, vernacularName, .after = elev.m)

ALA.ssp.niphophila = ALA.ssp.elev |>
  filter(infraspecificEpithet == "niphophila") |>
  filter(is.na(elevFlag))
  relocate(raw_scientificName, scientificName, vernacularName, .after = elev.m)
