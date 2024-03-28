library(tidyverse)
library(readxl)

# Calculate basal area for permanent plots ----
basal.area.fn <- function(x){ (pi*(x)^2)/40000 } # calculate basal area in m^2

# 2023 stem data ----
## Calculate 2023 basal area for each stem ----
tree.data.basalArea = read.csv("clean_data/Tree data.csv") |>
  select(plot:number, dbh) |>
  mutate(stemID = paste0(plot, ".", tree, ".", stem)) |>
  select(stemID, dbh) |>
  group_by(stemID) |>
  mutate(basal.area = basal.area.fn(dbh))

## Prepare 2023 stem data -----
tree.data.canopy = read.csv("clean_data/Tree data.csv") |>
  select(plot:number, heightM, flag, dbh, canopy, galleries, frass, barkDamage) |>
  # Assign numerical value to canopy
  mutate(canopy.number = case_when(
    canopy == "e" ~ 5,
    canopy == "g" ~ 4,
    canopy == "f" ~ 3,
    canopy == "p" ~ 2,
    canopy == "n" ~ 1
  )) |>
  # Beetle damage
  rename(puckering = barkDamage) |>
  # Turn binary
  mutate(frass = as.numeric(replace_na(frass, 0)),
         puckering = case_when(
           puckering %in% c("pA", "pL", "pU") ~ 1,
           TRUE ~ 0
         ),
         galleries = case_when(
           galleries %in% c("gA", "gL", "gU") ~ 1,
           TRUE ~ 0
         ),
         beetlesum = case_when(
           frass == 0 & puckering == 0 & galleries == 0 ~ 0,
           TRUE ~ 1
         )) |>
  # Add in basal area
  mutate(stemID = paste0(plot, ".", tree, ".", stem)) |>
  left_join(tree.data.basalArea)

# Prep 2024 data ----
## Calculate 2024 basal area per stem ----

tree.data.2024 = read_excel("raw_data/SOSG tree data 2024.xlsx", sheet = "data") |>
  # Fix plotNr
  mutate(plotNr = round(as.numeric(plotNr))) |>
  # Filter out the burned plot
  mutate(plotNickname = str_trim(plotNickname),
    plot = case_when(
      plotNickname == "Burning Log" ~ 23,
      burnHistory == "unburned" & health == "moderate" & plotNr == 1 ~ 21,
      burnHistory == "unburned" & health == "moderate" & plotNr == 2 ~ 22,
      burnHistory == "unburned" & health == "excellent" & plotNr == 1 ~ 7.2,
      burnHistory == "burned" & plotNr == 1 ~ 32,
      plotNickname == "Aqueduct" ~ 26,
      plotNickname == "Charlotte Pass" ~ 29,
      plotNickname == "2K" ~ 27,
      plotNickname == "Swing Bridge" ~ 24,
      plotNickname == "Snowies Alpine" ~ 28,
      plotNickname == "Geebung" ~ 30,
      plotNickname == "Perisher" ~ 31,
      plotNickname == "Blue Cow Lift" ~ 25,
      plotNr == 481 ~ 481,
      plotNr == 484 ~ 484,
      plotNickname == "SE-Satellite East" ~ 387,
      plotNickname == "SW-Satellite West" ~ 403,
      plotNickname == "SS-Satellite South" ~ 419,
      TRUE ~ NA
    )) |>
  # Resolve funny formatting of dendroNr
  mutate(dendroNr = case_when(
    dendroNr == "15?" ~ "15",
    TRUE ~ dendroNr),
    dendroNr = round(as.numeric(dendroNr), 1)) |>
    # Correct dendro numbers
  mutate(dendroNr = case_when(
      burnHistory == "unburned" & health == "excellent" & plotNr == 1 & treeNr > 10 &
        dendroNr %in% c(20, 26, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42) ~ dendroNr + 100,
    TRUE ~ dendroNr
    )) |>
  # Filter out erroneous data entry
  mutate(flag = case_when(
    dendroNr == 90 & treeNr == 34 ~ "data entry error",
    plot == 22 & treeNr == 51 ~ "wrong plot"
    TRUE ~ "okay"
  )) |>
  filter(flag == "okay")

table(tree.data.2024$plot)
table(tree.data.2024$plotNickname)

tree.data.2024.basalArea = tree.data.2024 |>
  drop_na(plot) |>
  mutate(stemID = paste0(plot, ".", treeNr, ".", stemNr)) |>
  select(stemID, dbh) |>
  group_by(stemID) |>
  mutate(basal.area = basal.area.fn(dbh)) |>
  filter(!is.na(basal.area))

## Prepare 2024 stem data ----
tree.data.2024.canopy = tree.data.2024 |>
  drop_na(plot) |>
  select(plotNickname, burnHistory:plotNr, plot, treeNr:hollows, canopy, frass:galleries, GPS, dendroNr, dendroType) |>
  rename(tree = treeNr, stem = stemNr, live = aliveStatus,
         bark = barkStatus, number = hollows) |>
  # Assign numerical value to canopy
  mutate(canopy.number = case_when(
    canopy == "excellent" ~ 5,
    canopy == "good" ~ 4,
    canopy == "fair" ~ 3,
    canopy == "poor" ~ 2,
    canopy == "none" ~ 1
  )) |>
  # Beetles
  # Turn binary
  mutate(frass = case_when(
    frass == "no" ~ 0,
    frass == "yes" ~ 1
  ),
  puckering = case_when(
    puckering == "none" ~ 0,
    TRUE ~ 1
  ),
  galleries = case_when(
    galleries == "none" ~ 0,
    TRUE ~ 1
  ),
  beetlesum = case_when(
    frass == 0 & puckering == 0 & galleries == 0 ~ 0,
    TRUE ~ 1)) |>
  select(-plotNr) |>
  # Add basal area
  mutate(stemID = paste0(plot, ".", tree, ".", stem)) |>
  left_join(tree.data.2024.basalArea) |>
  drop_na(basal.area)

# Combine both 2023 and 2024 stem data -----
tree.data.all.canopy = tree.data.canopy |>
  mutate(live = as.character(live),
         bark = as.character(bark),
         burnt = as.character(burnt)) |>
  bind_rows(tree.data.2024.canopy) |>
  distinct()

# write.csv(tree.data.all.canopy, "outputs/2024.03.27_Permanent plot trees for Weerach.csv")

# Figure out basal area for all plots ----

tree.data.BA.sum = tree.data.all.canopy |>
  group_by(plot) |>
  summarise(basal.area.total = sum(basal.area, na.rm = TRUE))

# Make dataset that includes plot basal area ----

tree.data.all = tree.data.all.canopy |>
  left_join(tree.data.BA.sum) |>
  mutate(
    # What percent of plot basal area does this stem take up?
    basal.area.proportion = basal.area/basal.area.total,
    # Multiply the canopy value (1-5) or beetle number (0 or 1)
    canopy.scaled = canopy.number * basal.area.proportion,
    beetle.scaled = beetlesum * basal.area.proportion)

# Take a look at the distribution of values -----
ggplot(tree.data.all.canopy, aes(x = as.character(plot), y = canopy.number)) +
  geom_violin(alpha = 0.7) +
  theme_bw()

beetle.check = tree.data.all |>
  # Sum up totals per plot
  group_by(plot) |>
  summarise(scaled.canopy.class = round(sum(canopy.scaled, na.rm = TRUE), 3),
            scaled.beetle.class = round(sum(beetle.scaled), 3))

ggplot(beetle.check, aes(x = scaled.beetle.class)) +
  geom_histogram(color = "white") +
  theme_bw()

# Scale health by basal area and assign plot classes ----
canopy.data.classes = tree.data.all |>
  # Filter out the old plot 7
  filter(plot != 7) |>
  # Sum up totals per plot
  group_by(plot, plotNickname) |>
    summarise(scaled.canopy.class = round(sum(canopy.scaled, na.rm = TRUE), 3),
              scaled.beetle.class = round(sum(beetle.scaled), 3)) |>
  # Assign classes
  mutate(beetle_category = case_when(
    scaled.canopy.class >= 3.5 & scaled.beetle.class < 0.5 ~ "Healthy canopy w/ few beetles",
    scaled.canopy.class < 3.5 & scaled.canopy.class > 2 & scaled.beetle.class > 0.5 ~ "Moderate canopy w/ many beetles",
    scaled.canopy.class <= 2 & scaled.beetle.class > 0.5 ~ "Dead canopy w/ many beetles",
    scaled.canopy.class < 3.5 & scaled.canopy.class > 2 & scaled.beetle.class < 0.5 ~ "Moderate canopy w/ few beetles"
  ),
  # Canopy loss
  canopy_category = case_when(
    scaled.canopy.class >= 3.5 ~ "Healthy canopy",
    scaled.canopy.class < 3.5 & scaled.canopy.class > 2 ~ "Moderate canopy loss",
    scaled.canopy.class <= 2 ~ "Severe canopy loss"
  )
  ) |>
  mutate(plot = round(plot))

# write.csv(canopy.data.classes, "outputs/2024.03.27_PlotClasses_BasalAreaScaling.csv")

table(canopy.data.classes$canopy_category)

hist(canopy.data.classes$scaled.canopy.class, breaks = 20)

# Visualise canopy health as a continuous scale ----
ggplot(canopy.data.classes |> filter(plot < 40), aes(y = plot, x = scaled.canopy.class, colour = canopy_category)) +
  geom_point(size = 4) +
  scale_colour_manual(values = c("dodgerblue3", "gold2", "firebrick3")) +
  labs(x = "Mean canopy health scaled by basal area", y = "Plot number") +
  theme_bw() +
  theme(axis.title = element_text(size = 16))

ggsave("outputs/2024.03.27_canopyClassContinuous.png", width = 8, height = 6, units = "in")

# Compare canopy health with binary beetles ----
tree.data.canopy = tree.data.all |>
  # average affect on trees
  group_by(plot, tree) |>
  summarise(tree.canopy = mean(canopy.number), tree.beetle = max(beetlesum))

ggplot(tree.data.canopy, aes(x = tree.canopy, fill = as.factor(tree.beetle))) +
  geom_histogram(color = "white", position = "dodge") +
  scale_fill_discrete(name = "Beetle detected", labels = c("No", "At least one stem")) +
  labs(x = "Dead to healthy canopy") +
  theme_bw()

ggsave("visualizations/2024.02.14_BeetlesCanopyFreq.png", width = 10, height = 5, units = "in")

# Calculate classes for Sabina's transects ----
# basal area function is defined at the head of this script
transect.data.basalArea = read.csv("clean_data/transect tree data.csv") |>
  select(transectNr:stem, dbh) |>
  mutate(stemID = paste0(transectNr, ".", elevation, ".", tree, ".", stem)) |>
  select(stemID, dbh) |>
  group_by(stemID) |>
  mutate(basal.area = basal.area.fn(dbh))

transect.data.canopy = read.csv("clean_data/transect tree data.csv") |>
  select(transectNr:stem, dbh, canopy, galleries, frass, barkDamage) |>
  # Assign numerical value to canopy
  mutate(canopy.number = case_when(
    canopy == "e" ~ 5,
    canopy == "g" ~ 4,
    canopy == "f" ~ 3,
    canopy == "p" ~ 2,
    canopy == "n" ~ 1
  )) |>
  # Beetle damage
  rename(puckering = barkDamage) |>
  # Turn binary
  mutate(frass = as.numeric(replace_na(frass, 0)),
         puckering = case_when(
           puckering %in% c("pA", "pL", "pU") ~ 1,
           TRUE ~ 0
         ),
         galleries = case_when(
           galleries %in% c("gA", "gL", "gU") ~ 1,
           TRUE ~ 0
         ),
         beetlesum = case_when(
           frass == 0 & puckering == 0 & galleries == 0 ~ 0,
           TRUE ~ 1
         )) |>
  # Add in basal area
  mutate(stemID = paste0(transectNr, ".", elevation, ".", tree, ".", stem)) |>
  left_join(transect.data.basalArea)

# Sum up the basal area by transect
transect.data.BA.sum = transect.data.canopy |>
  group_by(transectNr, elevation) |>
  summarise(basal.area.total = sum(basal.area, na.rm = TRUE))

# Make an object with all the areas and data
transect.data.all = transect.data.canopy |>
  left_join(transect.data.BA.sum) |>
  mutate(
    # What percent of plot basal area does this stem take up?
    basal.area.proportion = basal.area/basal.area.total,
    # Multiply the canopy value (1-5) or beetle number (0 or 1)
    canopy.scaled = canopy.number * basal.area.proportion,
    beetle.scaled = beetlesum * basal.area.proportion)

# Check beetle divide
beetle.check = transect.data.all |>
  # Sum up totals per plot
  group_by(transectNr, elevation) |>
  summarise(scaled.canopy.class = round(sum(canopy.scaled, na.rm = TRUE), 3),
            scaled.beetle.class = round(sum(beetle.scaled), 3))

ggplot(beetle.check, aes(x = scaled.beetle.class)) +
  geom_histogram(color = "white") +
  theme_bw()

# Calculate classes
transect.data.classes = transect.data.all |>
  # Sum up totals per plot
  group_by(transectNr, elevation) |>
  summarise(scaled.canopy.class = round(sum(canopy.scaled, na.rm = TRUE), 3),
            scaled.beetle.class = round(sum(beetle.scaled, na.rm = TRUE), 3)) |>
  # Assign classes
  mutate(class = case_when(
    scaled.canopy.class >= 3.5 & scaled.beetle.class < 0.5 ~ "low severity",
    scaled.canopy.class < 3.5 & scaled.canopy.class > 2 & scaled.beetle.class > 0.5 ~ "moderate severity",
    scaled.canopy.class <= 2 & scaled.beetle.class >= 0.5 ~ "high severity",
    scaled.canopy.class < 3.5 & scaled.beetle.class < 0.5 ~ "high canopy death w/ few beetles",
    scaled.canopy.class >= 3.5 & scaled.beetle.class >= 0.5 ~ "low canopy death w/ many beetles"
  ))

# write.csv(transect.data.classes, "outputs/2024.02.15_SabinaTransectsClassed.csv")


# Archive versions of stem.data.classes ----
#
# stem.data.classes = stem.data.all |>
#   group_by(plot) |>
#   summarise(mean.canopy.class = mean(canopy.number, na.rm = TRUE), mean.beetle.class = mean(beetlesum, na.rm = TRUE)) |>
#   mutate(class = case_when(
#     canopy.scale >= 3 & mean.beetle.class < 0.25 ~ "low severity",
#     mean.canopy.class < 3 & mean.canopy.class > 2 & mean.beetle.class > 0.25 ~ "moderate severity",
#     mean.canopy.class <= 2 & mean.beetle.class > 0.25 ~ "high severity",
#     mean.canopy.class < 3 & mean.beetle.class < 0.25 ~ "high canopy death w/ few beetles"
#   ))
#
# ggplot(canopy.data.classes, aes(x = plot, y = mean.canopy.class)) +
#   geom_point() +
#   theme_bw()
#
# # Redo the above but with canopy class colors
# canopy.data.classed = canopy.data |>
#   left_join(canopy.data.classes) |>
#   mutate(plot = factor(plot))
#
# library(viridis)
# ggplot(canopy.data.classed,
#        aes(x = plot, y = canopy.number,
#            fill = mean.canopy.class, color = mean.canopy.class)) +
#   geom_violin(alpha = 0.7) +
#   scale_fill_viridis(direction = -1) +
#   scale_color_viridis(direction = -1) +
#   theme_bw()
#
# ggsave("visualizations/2024.01.03_canopyClass_meanCanopyClass.png", width = 10, height = 4, units = "in")
#
#
# # Beetle affected -----
# tree.data.beetle = read.csv("clean_data/Tree data.csv") |>
#   select(plot:number, dbh, galleries, frass, barkDamage) |>
#   rename(puckering = barkDamage) |>
#   # Turn binary
#   mutate(frass = as.numeric(replace_na(frass, 0)),
#   puckering = case_when(
#     puckering %in% c("pA", "pL", "pU") ~ 1,
#     TRUE ~ 0
#   ),
#   galleries = case_when(
#     galleries %in% c("gA", "gL", "gU") ~ 1,
#     TRUE ~ 0
#   ),
#   beetlesum = case_when(
#     frass == 0 & puckering == 0 & galleries == 0 ~ 0,
#     TRUE ~ 1
#   ))
#
# tree.data.2024.beetle = read.csv("raw_data/SOSG tree data 2024.csv") |>
#   select(burnHistory:plotNr, treeNr:hollows, frass:galleries) |>
#   rename(tree = treeNr, stem = stemNr, live = aliveStatus,
#          bark = barkStatus, number = hollows) |>
#   # Turn binary
#   mutate(frass = case_when(
#     frass == "no" ~ 0,
#     frass == "yes" ~ 1
#   ),
#   puckering = case_when(
#     puckering == "none" ~ 0,
#     TRUE ~ 1
#   ),
#   galleries = case_when(
#     galleries == "none" ~ 0,
#     TRUE ~ 1
#   ),
#   beetlesum = case_when(
#     frass == 0 & puckering == 0 & galleries == 0 ~ 0,
#     TRUE ~ 1),
#   plot = case_when(
#     burnHistory == "unburned" & health == "moderate" & plotNr == 1 ~ 21,
#     burnHistory == "unburned" & health == "moderate" & plotNr == 2 ~ 22,
#     burnHistory == "unburned" & health == "excellent" & plotNr == 1 ~ 7.2,
#     TRUE ~ NA
#   )) |>
#   select(-plotNr) |>
#   drop_na(plot)
#
# stem.data.all = tree.data.beetle |>
#   mutate(live = as.character(live),
#          bark = as.character(bark),
#          burnt = as.character(burnt)) |>
#   bind_rows(tree.data.2024.beetle) |>
#   left_join(tree.data.all)
#
# stem.data.classes = stem.data.all |>
#   group_by(plot) |>
#   summarise(mean.canopy.class = mean(canopy.number, na.rm = TRUE), mean.beetle.class = mean(beetlesum, na.rm = TRUE)) |>
#   mutate(class = case_when(
#     canopy.scale >= 3 & mean.beetle.class < 0.25 ~ "low severity",
#     mean.canopy.class < 3 & mean.canopy.class > 2 & mean.beetle.class > 0.25 ~ "moderate severity",
#     mean.canopy.class <= 2 & mean.beetle.class > 0.25 ~ "high severity",
#     mean.canopy.class < 3 & mean.beetle.class < 0.25 ~ "high canopy death w/ few beetles"
#   ))
#
# ggplot(stem.data.classes, aes(x = mean.beetle.class)) +
#   geom_histogram(bins = 50, color = "white") +
#   geom_vline(xintercept = 0.1) +
#   theme_bw()
#
# # Visualise correlation between canopy health and beetles ----
# ggplot(stem.data.all, aes(x = canopy.number, fill = as.factor(beetlesum))) +
#   geom_histogram(color = "white", position = "dodge") +
#   theme_bw()
#
# tree.data = stem.data.all |>
#   # average affect on trees
#   group_by(plot, tree) |>
#   summarise(tree.canopy = mean(canopy.number), tree.beetle = mean(beetlesum)) |>
#   # join in classes
#   left_join(stem.data.classes |> select(plot, class))
#
# ggplot(tree.data, aes(x = tree.canopy, y = tree.beetle, color = class)) +
#   geom_point(size = 3) +
#   scale_colour_manual(values = c("black", "firebrick", "dodgerblue4", "orange"), name = "Plot class")+
#   labs(x = "Dead to healthy canopy", y = "No beetles to all beetles") +
#   theme_bw()
#
# ggsave("visualizations/2024.02.14_BeetleCanopyCorrelation.png", width = 10, height = 8, units = "in")
#
# tree.data.canopy = stem.data.all |>
#   # average affect on trees
#   group_by(plot, tree) |>
#   summarise(tree.canopy = mean(canopy.number), tree.beetle = max(beetlesum)) |>
#   # join in classes
#   left_join(stem.data.classes |> select(plot, class))
#
# ggplot(tree.data.canopy, aes(x = tree.canopy, fill = as.factor(tree.beetle))) +
#   geom_histogram(color = "white", position = "dodge") +
#   scale_fill_discrete(name = "Beetle detected", labels = c("No", "At least one stem")) +
#   labs(x = "Dead to healthy canopy") +
#   theme_bw()
#
# ggsave("visualizations/2024.02.14_BeetlesCanopyFreq.png", width = 10, height = 5, units = "in")
#
