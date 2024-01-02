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
  mutate(species = case_when(
    species == "Acriphylla simplicifolia" ~ "Aciphylla simplicifolia",
    species %in% c("Asperula gunii", "Asperulla gunii", "Asperulla gunnii") ~ "Asperula gunnii",
    species == "Acrothamnus maccrali" ~ "Acrothamnus maccraei",
    species == "Backea gunniana" ~ "Baeckea gunniana",
    species %in% c("Celmesia tomantella", "Celmisia tomentella", "Clemisia tomentella") ~ "Celmisia tomentella",
    species == "Curex" ~ "Carex",
    species == "Crespidia" ~ "Craspedia",
    species == "Empadisma minus" ~ "Empodisma minus",
    species == "Epachris?" ~ "Epacris?",
    species == "Grevillea australia" ~ "Grevillea australis",
    species == "Hovea montata" ~ "Hovea montana",
    species %in% c("Oriomyrrhis eriopoda", "Oreomyrrhis eriopoda", "Oreomyrrhis eriopoda") ~ "Oreomyrrhis eriopoda",
    species %in% c("Ozothamnus secandifolius", "Ozothamnus secundiflora", "Ozothamnus secundiflorus") ~ "Ozothamnus secundiflorus",
    species == "Orchid 1" ~ "Unknown orchid 1",
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
    TRUE ~ species
  ),
  species = str_replace(species, " sp.", ""),
  species = str_replace(species, "Oleria", "Olearia"),
  species = str_replace(species, "Celmecia", "Celmisia"),
  species = str_replace(species, "Celmesia", "Celmisia"),
  species = str_replace(species, "uk ", "Unknown "),
  species = str_to_sentence(trimws(species)))

table(subplot.data$species)

write.csv(subplot.data, "clean_data/5x5 subplot data.csv")
