threatened.spp = read.csv("raw_data/species_records-2024-09-13/records-2024-09-13.csv") |>
  select(dataResourceUid, basisOfRecord, raw_scientificName, scientificName, kingdom, phylum, class, order, family, genus, subgenus, specificEpithet,
         infraspecificEpithet, vernacularName) |>
  # Filter out the irrelevant basis of records
  mutate(flag = case_when(
    basisOfRecord %in% c("FOSSIL_SPECIMEN", "MACHINE_OBSERVATION", "OBSERVATION", "OCCURRENCE") ~ "discard",
    basisOfRecord == "HUMAN_OBSERVATION" ~ "suspect",
    TRUE ~ "okay"
  )) |>
  filter(flag == "okay") |>
  # Make list of species
  select(-c(dataResourceUid, basisOfRecord, raw_scientificName)) |>
  distinct()

threatened.spp.count = threatened.spp |>
  group_by(scientificName, flag) |>
  summarize(n = length(raw_scientificName))


write.csv(threatened.spp, "outputs/2024.09.13_ALA_Alps_ThreatenedSpp.csv")

# Compare with existing list

rubys.list = read.csv("raw_data/species_records-2024-09-13/Ruby Australian Alps Threatened Species List.csv") |>
  select(Scientific.name) |>
  rename(scientificName = Scientific.name) |>
  # mutate(list = "ruby") |>
  separate(scientificName, into = c("genus", "epithet"))

compare.lists = threatened.spp |>
  select(scientificName) |>
  separate(scientificName, into = c("genus", "epithet")) |>
  anti_join(rubys.list)
