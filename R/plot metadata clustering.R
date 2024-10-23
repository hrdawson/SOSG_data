library(GGally)

# Plot canopy class against all the other plot variables

# site.meta is from GIS extracted metadata.R

plot.meta = site.meta |>
  # Prepare site data (extracted by Weerach)
  filter(point == "c") |>
  rename(plot = plotNr) |>
  mutate(plot = as.numeric(plot)) |>
  select(-c("lon", "lat", "ele_30m")) |>
  # Join canopy data
  left_join(canopy.data.classes) |>
  # Join clim.meta from `extract data from worldclim.R`
  left_join(clim.meta) |>
  # remove irrelevant columns
  select(-c(placeholder, point, plotNickname, nickname, scaled.beetle.class, beetle_category,
            tot.prec, tavg, tmin, tmax)) |>
  relocate(scaled.canopy.class, canopy_category, latitude, longitude, .after = plot) |>
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) |>
  # make long for graphing
  pivot_longer(cols = ele_10m:temp.dry.q.mean, names_to = "variable", values_to = "value")


# Visualise
ggplot(plot.meta |> filter(plot <=20), aes(x = value, y = scaled.canopy.class, colour = scaled.canopy.class)) +
  geom_point() +
  facet_wrap(~variable, scales = "free") +
  scale_colour_viridis_c(direction = -1) +
  theme_bw()

ggsave("outputs/2024.07.30_canopyClass_vs_climateVariable_plots1-20.png", width = 14, height = 12, units = "in")

ggplot(plot.meta |> filter(variable == "ele_10m")|>filter(plot<=20), aes(x = longitude, y = latitude,
                                                       colour = value, shape = canopy_category)) +
  geom_point(size = 4) +
  scale_colour_viridis_c(option = "D", direction = -1, name = "Elevation (m)") +
  theme_bw()
ggsave("outputs/2024.07.30_canopyClass_vs_location_plots1-20.png")
