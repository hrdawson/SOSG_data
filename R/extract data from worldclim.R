#Extract historical climate data for anywhere in the world
#Code written by Hilary Rose Dawson, Dec 2020

##Download data from worldclim.org and unzip folder before use.
##Run the function separately for each climate variable (precip, Tavg, etc.).
##Use the join functions from dplyr to combine into one climate data set.

##This works with soil data too! Check out https://casoilresource.lawr.ucdavis.edu/soil-properties/download.php
##Note that these are old rasters that need to be exported as GeoTIFs for R to read them.
##QGIS Save As function works great for this.

#You will need:
##folderpath: folder address of downloaded worldclim data
####eg "worldclim/tavg"
##points: an object with latitude and longitude columns

#You will also need the following packages:
library(raster)
library(sp)
library(reshape)
library(purrr)


# Make points
points = read.csv("raw_data/permanent plot points 2023.csv") |>
  dplyr::rename(plotName = name) |>
  bind_rows(read.csv("raw_data/permanent plot points 2024.csv")) |>
  # select(id, plotName, longitude, latitude) |>
  # rename(plot = id) |>
  # left_join(read.csv("outputs/2024.07.23_canopy.data.classes.csv")) |>
  # select(-c(X, plotNickname)) |>
  # left_join(read.csv("outputs/worldclim_metadata.csv")) |>
  # drop_na(plot) |>
  # write.csv("outputs/2024.07.23_plot_metadata.csv")
  dplyr::rename(lon = longitude, lat = latitude) |>
  drop_na(lon) |>
  dplyr::select(id, plotName, lon, lat)

#Make 'points' into a spatial object.
##Replace "lon" and "lat" with appropriate column names.
sp::coordinates(points) = ~lon+lat

#Create functions for extracting climate data from rasters by location.
##Place cursor in first line of function and press 'run'.

##extract.clim is for all variables with multiple rasters (this is most variables)
extract.clim = function(folderpath, points) {
  #Read rasters
  files <- dir(folderpath, pattern = "*.tif")
  #Combine rasters into a rasterStack
  alldata <- files%>%
    purrr::map(~ raster(file.path(folderpath, .))) %>%
    reduce(stack)
  #extract raster data for each point
  data = raster::extract(alldata, points)
  #Calculate statistics
  datasum = data %>%
    melt() %>%
    group_by(X1) %>%
    summarize(total = sum(value),
              mean = mean(value),
              min = min(value),
              max = max(value)) %>%
    left_join(as.data.frame(points) %>% mutate(X1 = row_number()))

}

#basic.clim is for variables with a single raster (elevation)
#Again, put cursor in first line and run
basic.clim = function(folderpath, points) {
  #Read rasters
  files <- dir(folderpath, pattern = "*.tif")
  #Combine rasters into a rasterStack
  alldata <- files%>%
    purrr::map(~ raster(file.path(folderpath, .))) %>%
    reduce(stack)
  #extract raster data for each point
  data = alldata %>%
    raster::extract(points)%>%
    melt()%>%
    bind_cols(as.data.frame(points))
}

# bio.clim handles the different variables of the ANUCLIM file
folderpath = "raw_data/datasets/worldclim/ANUCLIM/"

bio.clim = function(folderpath, points) {
  #Read rasters
  files <- dir(folderpath, pattern = "*.tif")
  #Combine rasters into a rasterStack
  alldata <- files%>%
    purrr::map(~ raster(file.path(folderpath, .))) %>%
    reduce(stack)
  #extract raster data for each point
  data = raster::extract(alldata, points) |>
    bind_cols(as.data.frame(points)) |>
    as.data.frame() |>
    # Rename the variables
    # See https://worldclim.org/data/bioclim.html for key
    dplyr::rename(temp.mean = wc2.1_30s_bio_1, diurnal.range.mean = wc2.1_30s_bio_2, isotherm = wc2.1_30s_bio_3,
                  temp.seasonality = wc2.1_30s_bio_4, temp.warm.m.mean = wc2.1_30s_bio_5, temp.cold.m.mean = wc2.1_30s_bio_6,
                  temp.range = wc2.1_30s_bio_7, temp.wet.q.mean = wc2.1_30s_bio_8, temp.dry.q.mean = wc2.1_30s_bio_9,
                  temp.warm.q.mean = wc2.1_30s_bio_10, temp.cold.q.mean = wc2.1_30s_bio_11, precip = wc2.1_30s_bio_12,
                  precip.wet.m.mean = wc2.1_30s_bio_13, precip.dry.m.mean = wc2.1_30s_bio_14,
                  precip.seasonality = wc2.1_30s_bio_15, precip.wet.q.mean = wc2.1_30s_bio_16, precip.dry.q.mean = wc2.1_30s_bio_17,
                  precip.warm.q.mean = wc2.1_30s_bio_18, precip.cold.q.mean = wc2.1_30s_bio_19
                  )
}

#Then use the functions as if they came in a CRAN package
#Examples:
prec = extract.clim("raw_data/datasets/worldclim/precip/", points)%>%
  #Create unique ID for each point
  unite(lat_lon, c("lat", "lon"))%>%
  #Select just the relevant columns
  ##notice that precip. uses total, not mean
  dplyr::select(total, lat_lon)%>%
  #Rename so you know total what
  dplyr::rename(tot.prec = total)

vpd = extract.clim("raw_data/datasets/worldclim/vapr/", points)%>%
  unite(lat_lon, c("lat", "lon"))%>%
  ##Notice that vpd uses mean, not total
  dplyr::select(mean, lat_lon)%>%
  dplyr::rename(vpd = mean)

tavg = extract.clim("raw_data/datasets/worldclim/tavg/", points)%>%
  unite(lat_lon, c("lat", "lon"))%>%
  ##Notice that Tavg uses mean, not total
  dplyr::select(mean, lat_lon)%>%
  dplyr::rename(tavg = mean)

tmin = extract.clim("raw_data/datasets/worldclim/tmin/", points)%>%
  unite(lat_lon, c("lat", "lon")) |>
  ##Notice that Tmin uses the minimum, not mean
  ## REVISIT THIS
  dplyr::select(min, lat_lon)%>%
  dplyr::rename(tmin = min)

tmax = extract.clim("raw_data/datasets/worldclim/tmax/", points)%>%
  unite(lat_lon, c("lat", "lon"))%>%
  ##Notice that Tmax uses maximum, not mean
  ## REVISIT THIS
  dplyr::select(max, lat_lon)%>%
  dplyr::rename(tmax = max)

elev = basic.clim("raw_data/datasets/worldclim/elev/", points) |>
  unite(lat_lon, c("lat", "lon")) |>
  dplyr::rename(elev = value)

ANUCLIM = bio.clim("raw_data/datasets/worldclim/ANUCLIM/", points) |>
  unite(lat_lon, c("lat", "lon"))

#Once extracted, you can combine all the data into one metadata object.
clim.meta = points %>%
  as.data.frame()%>%
  unite(lat_lon, c("lat", "lon"))%>%
  inner_join(prec)%>%
  inner_join(vpd) |>
  inner_join(tavg)%>%
  inner_join(tmin) |>
  inner_join(tmax) |>
  inner_join(elev) |>
  inner_join(ANUCLIM) |>
  separate(lat_lon, into = c("latitude", "longitude"), sep = "_") |>
  dplyr::rename(plot = id, nickname = plotName)

write.csv(clim.meta, "outputs/worldclim_metadata.csv")
