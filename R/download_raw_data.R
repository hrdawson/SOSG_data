# Download data using this script
# Use RStudio's handy script menu to jump to the data you need
# You should only need to download data once, unless you know they have been updated
# If you've been away from the project for awhile, you should download them again
# install.packages("remotes")
# remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

# Soils data ----
get_file(
  # Which repository is it in?
  node = "afk2p",
  # Which file do you want?
  file = "Brookhouse 2023 Project Spreadsheet Soil analyses.xlsx",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/soils")

get_file(
  # Which repository is it in?
  node = "afk2p",
  # Which file do you want?
  file = "Matt's Soil bulk density final.xlsx",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/soils")

get_file(
  # Which repository is it in?
  node = "afk2p",
  # Which file do you want?
  file = "Rekap Data_Matt-Phosphorus Soil.xlsx",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/soils")

# Canopy data ----
get_file(
  # Which repository is it in?
  node = "afk2p",
  # Which file do you want?
  file = "SOSG tree data 2024.xlsx",
  # Where do you want the file to go to?
  path = "raw_data",
  # Where is the file stored within the OSF repository?
  remote_path = "raw_data/permanentPlot")
