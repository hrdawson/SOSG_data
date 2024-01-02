library(vegan)

composition = read.csv("clean_data/5x5 subplot data.csv") |>
  # Some obs have duplicate records for one species in a plot
  # Average them
  group_by(plot, midlineDistance, species) |>
  summarise(coverPercent = mean(coverPercent)) |>
  ungroup() |>
  # Put in a format that vegan understands
  pivot_wider(names_from = species, values_from = coverPercent) |>
  mutate_all(funs(replace_na(., 0))) |>
  mutate(id = paste0(plot, ".", midlineDistance)) |>
  select(-c(plot, midlineDistance)) |>
  column_to_rownames(var = "id") |>
  # Calculate Shannons
  mutate(shannon = diversity(composition, index = "shannon"))

# Something is up with the rownames to column idea
# Each time I add it, it breaks the `shannon` calculation **even though that calculation is before this code**
# The solution is to remove the pipe after column_to_rownames, run it, add the pipe to the mutate, and run again
# I'm making this a new object so that I can bring it back into useable form

composition.shannon = composition |>
  rownames_to_column(var = "id") |>
  mutate(shannon.H = as.numeric(shannon)) |>
  select(id, shannon.H) |>
  separate(id, into = c("plot", "midlineDistance")) |>
  # From Canopy health classes.R
  left_join(canopy.data.classed)

composition.shannon.mean = composition.shannon |>
  group_by(plot) |>
  summarise(mean.H = mean(shannon.H))

composition.canopy = canopy.data.classed |>
  left_join(composition.shannon.mean) |>
  mutate(plot = factor(as.numeric(plot)))

# Visualise
ggplot(composition.shannon, aes(x = mean.canopy.class, y = shannon.H)) +
  geom_point() +
  theme_bw()

library(viridis)
ggplot(composition.canopy,
       aes(x = fct_reorder(plot, mean.canopy.class), y = canopy.number,
           fill = mean.H, color = mean.H)) +
  geom_violin(alpha = 0.7) +
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  theme_bw()

ggsave("visualizations/2024.01.03_canopyClass_Shannon.png", width = 10, height = 4, units = "in")
