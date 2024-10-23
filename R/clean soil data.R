# Soils data cleaning
library(tidyverse)
library(readxl)
library(janitor)
library(tidylog)

# Clean the soil chemistry data ----
soil.chem.data = read_excel("raw_data/Brookhouse 2023 Project Spreadsheet Soil analyses.xlsx",
                     skip = 1) |>
  # Dates and times will need fixing, but that's a problem for another day
  clean_names("upper_camel", replace=janitor:::mu_to_u) |>
  # Make sure NAs are read in
  mutate(across(where(is.character), ~na_if(., "n/a"))) |>
  # Filter to just soil chemical analyses
  filter(SampleType == "Soil") |>
  # Remove columns with no data (from https://community.rstudio.com/t/drop-all-na-columns-from-a-dataframe/5844/2)
  map(~.x) %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x) |>
  # Rename columns now that we know which ones we'll use
  rename(Elevation.m = ElevationM, LitterDepth.cm = LitterDepthCm, VegSpp = VegetationDescription, GrindingTime = Grinding, pH = PH1_5Water, EC = Ec1_5Water,
         CN.sample.g = CnSampleMassG, TC.percent = Tc, TN.percent = Tn, CN = CNRatio, TIC.sample.g = TicSampleMassG, TIC.percent = Tic,
         TOC.percent = Toc, TP.sample.g = TpDigestionSampleMassG, TP.percent = TpMgPL) |>
  # Select just the data relevant to analyses
  select(SampleNo:Elevation.m, LitterDepth.cm, VegSpp, pH, EC, CN.sample.g:CN, TIC.sample.g:TOC.percent, TP.sample.g, TP.percent)

write.csv(soil.chem.data, "clean_data/transect soil data.csv")

# Clean the bulk density data ----
soil.bd.data = read_excel("raw_data/Brookhouse 2023 Project Spreadsheet Soil analyses.xlsx",
                          skip = 1) |>
  # Dates and times will need fixing, but that's a problem for another day
  clean_names("upper_camel", replace=janitor:::mu_to_u) |>
  # Make sure NAs are read in
  mutate(across(where(is.character), ~na_if(., "n/a"))) |>
  # Filter to just soil chemical analyses
  filter(SampleType == "BDC") |>
  # Remove columns with no data (from https://community.rstudio.com/t/drop-all-na-columns-from-a-dataframe/5844/2)
  map(~.x) %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x) |>
  # Rename columns now that we know which ones we'll use
  rename(Elevation.m = ElevationM, BDC.vol.cm3 = BdcVolumeCm3, BDC.sample.dry.g = DryWeightG, BDC.sample.wet.g = WetWeightG, SoilMoisture.percent = MoistureContent,
         BD.dry.g.cm3 = DryBulkDensityGCm3, BD.wet.g.cm3 = WetBulkDensityGCm3) |>
  # Select just the data relevant to analyses
  select(SampleNo, SampleLabel, Transect, Elevation.m, BDC.vol.cm3, BDC.sample.dry.g:BD.wet.g.cm3)

write.csv(soil.bd.data, "clean_data/transect bulk density data.csv")
