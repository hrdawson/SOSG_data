# 5x5 Plot data manager
library(readxl)
library(janitor)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(tidylog)

# Read in files ----
# Make file list
filesSubplot <- dir(path = "raw_data/fixed area plot (permanent plot) data", pattern = ".xlsx",
                    full.names = TRUE, recursive = TRUE)

# Read in data
# If you get an error that `~$Plot **.xlsx` cannot be opened, close all Excel documents and try both objects again
tempSubplot = map_dfr(filesSubplot, read_xlsx, sheet = "5 x 5 subplots",
                      range = cell_cols("A:K"))

# Clean data ----
subplot.data = tempSubplot |>
  # Correct row names
  row_to_names(row_number = 1) |>
  clean_names(case = "lower_camel") |>
  select(-na) |>
  # Some data entry was not completed
  # THIS STEP IS DANGEROUS, check here first for errors
  fill(plot, .direction = "down") |>
  # Now we can group by plot
  group_by(plot) |>
  fill(assessor1:date, .direction = "down") |>
  ungroup() |>
  # Filter out extraneous row names
  filter(plot != "Plot") |>
  # Remove filler data
  filter(assessor1 != assessor2) |>
  drop_na(species) |>
  # Convert non-numeric values to numeric
  # For less than 5% cover, replace value with trace amount number
  mutate(coverPercent = str_replace(coverPercent, "<5", "0.1"),
         coverPercent = str_replace(coverPercent, ">5", "0.1"),
         # Abundance is trickier because it's a large size class
         # IE <1000 means between 500 and 1000
         # But running on the idea of relative proportion
         abundance = case_when(
           abundance == "<10" ~ "10",
           abundance == "<100" ~ "100",
           abundance == "<300" ~ "300",
           abundance == "<1000" ~ "999",
           abundance == ">1000" ~ "1000",
           abundance == "<20" ~ "20",
           abundance == "<5" ~ "5",
           abundance == "<50" ~ "50",
           abundance == "<500" ~ "500",
           TRUE ~ abundance
         ),
         # Convert date to human readable
         # Someone didn't enter dates in a standard way
         date = str_replace(date, "19.01.23", "44945"),
         date = openxlsx::convertToDate(date)) |>
  # Convert relevant columns to numeric
  mutate_at(c("midlineDistance", "sample", "coverPercent", "abundance",
              "cwdLengthM"), as.numeric) |>
  # Disambiguate species names
  mutate(
    species = str_replace(species, " sp.", ""),
    species = str_replace(species, "Celmecia", "Celmisia"),
    species = str_replace(species, "Celmesia", "Celmisia"),
    species = str_replace(species, "Oleria", "Olearia"),
    species_resolved = case_when(
    species == "Acriphylla simplicifolia" ~ "Aciphylla simplicifolia",
    species %in% c("Asperula gunii", "Asperulla gunii", "Asperulla gunnii") ~ "Asperula gunnii",
    species == "Acrothamnus maccrali" ~ "Acrothamnus maccraei",
    species == "Backea gunniana" ~ "Baeckea gunniana",
    species == "Baeckea?" ~ "Baeckea",
    species %in% c("Celmesia tomantella", "Celmisia tomentella",
                   "Clemisia tomentella", "Celmisia tomantella") ~ "Celmisia tomentella",
    species == "Celmisia Pugioniformis" ~ "Celmisia pugioniformis",
    species %in% c("Curex", "Carex?") ~ "Carex",
    species == "Crespidia" ~ "Craspedia",
    species == "Empadisma minus" ~ "Empodisma minus",
    species %in% c("Epachris?", "Apacras?") ~ "Epacris",
    species == "Grevillea australia" ~ "Grevillea australis",
    species %in% c("Hovea montata", "Hovea Montana") ~ "Hovea montana",
    species %in% c("Oriomyrrhis eriopoda", "Oreomyrrhis eriopoda", "Oreomyrrhis eriopoda") ~ "Oreomyrrhis eriopoda",
    species %in% c("Ozothamnus secandifolius", "Ozothamnus secundiflora", "Ozothamnus secundiflorus") ~ "Ozothamnus secundiflorus",
    species == "Orchid 1" ~ "Orchidaceae",
    species == "Pterostylis (orchid)" ~ "Pterostylis",
    species == "Pimelea lingustrina" ~ "Pimelea ligustrina",
    species == "Pimelia axiflora" ~ "Pimelea axiflora",
    species == "Poa ensiformus" ~ "Poa ensiformis",
    species == "Poa siebriana" ~ "Poa sieberiana",
    species == "Podocarpus lawrenci" ~ "Podocarpus lawrencei",
    species == "Polystichum prolierum" ~ "Polystichum proliferum",
    species == "Richea continentus" ~ "Richea continentis",
    species == "Senecio gunii" ~ "Senecio gunnii",
    species == "Stelaria pungens" ~ "Stellaria pungens",
    species == "Tasmania xerophila" ~ "Tasmannia xerophila",
    species == "Unknown pimelea"~ "Pimelea",
    species == "Unknown Orchid (Caladenia)" ~ "Caladenia",
    species %in% c("Viola bentonicifolia", "Viola betiusefola", "Viola betonicfolia", "Viola betonicifolia") ~ "Viola betonicifolia",
    species == "Carraway" ~ "Oreomyrrhis eriopoda",
    species == "Sorrel" ~ "Acetosella vulgaris",
    species %in% c("Olearia brevipunctata", "Olearia brevipunctata") ~ "Olearia brevipedunculata",
    species == "Lily" ~ "Liliaceae",
    species == "Probably acrothamnus- Red" ~ "Acrothamnus",
    species %in% c("Sedge?", "Unknown sedge") ~ "Cyperaceae",
    species == "Seneciocies 2" ~ "Senecio",
    species == "Unknown geranium" ~ "Geranium",
    species == "uk mallee" ~ "Eucalyptus",
    species %in% c("Unknown grass (flag)", "Unknown grass (furry)", "Unknown Grass 1", "Unknown Grass 2",
                   "Unknown Grass 3", "Unknown grass 1", "Unknown grass 2", "Unknown Grass- HAiry", "Unknown Grass", "uk Poa") ~ "Poaceae",
    species %in% c("uk asteracae", "Unknown asteraceae", "Unknown daisy") ~ "Asteraceae",
    species %in% c("long leaf forb", "Unknown pocket forb", "Pocket Forb", "Unknown forb", "Unknown forb (cutie)", "Unknown forb 1",
                   "Unknown shrub (faekea)", "Unknown shrub", "Unknown spider thing?", "Unknownder thing?",
                   "Unknown shrub 1", "cushion thing?") ~ "Unknown",
    TRUE ~ species
  ),
  species = str_replace(species, "uk ", "Unknown "),
  species = str_to_sentence(trimws(species))) |>
  # Remove duplicates of the same species in the same plot
  distinct() |>
  rename(species_field = species, species = species_resolved)

table(subplot.data$species_field)
table(subplot.data$species)

write.csv(subplot.data, "clean_data/5x5 subplot data.csv")
