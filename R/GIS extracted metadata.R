# Read in data
# Made by Weerach
# Notes:
# The slope and aspect values are extracted at each point (start-end-center).
# They are calculated from resampled DEM map, resampled to 10m and 30m pixel raster maps.
# If you want to look at the overall landform, like using slope and aspect to explain effect of solar radiation etc.
# I think 30m resolution works better.
# If you want to use it to explain hydrological features, like flow accumulation,
# higher resolution is usually preferred.
#
# Source of original DEM data
# https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/89644

site.meta = read.csv("raw_data/permanent-plot_ele-slope-aspect/kosci_pp_start-end-center_epsg4326_ele-slope-aspect_2.csv") |>
  separate(plot_name, into = c("placeholder", "plotNr", "point"))

write.csv(site.meta, "outputs/plotMetadata.csv")
