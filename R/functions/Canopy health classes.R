canopy.data = read.csv("clean_data/Tree data.csv") |>
  select(plot:number, heightM, flag, canopy) |>
  # Assign numerical value to canopy
  mutate(canopy.number = case_when(
    canopy == "e" ~ 5,
    canopy == "g" ~ 4,
    canopy == "f" ~ 3,
    canopy == "p" ~ 2,
    canopy == "n" ~ 1
  ))

# Take a look at the distribution of values
ggplot(canopy.data, aes(x = as.character(plot), y = canopy.number)) +
  geom_violin(alpha = 0.7) +
  theme_bw()


# Try to divide into three classes
canopy.data.classes = canopy.data |>
  group_by(plot) |>
  summarise(mean.canopy.class = mean(canopy.number, na.rm = TRUE))

ggplot(canopy.data.classed, aes(x = plot, y = mean.canopy.class)) +
  geom_point() +
  theme_bw()

# Redo the above but with canopy class colors
canopy.data.classed = canopy.data |>
  left_join(canopy.data.classes) |>
  mutate(plot = factor(plot))

library(viridis)
ggplot(canopy.data.classed,
       aes(x = plot, y = canopy.number,
           fill = mean.canopy.class, color = mean.canopy.class)) +
  geom_violin(alpha = 0.7) +
  scale_fill_viridis(direction = -1) +
  scale_color_viridis(direction = -1) +
  theme_bw()
