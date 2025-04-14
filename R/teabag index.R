# Teabag analyses
## Read in data--this is a bit complicated
library(openxlsx2)

teabag.test = read_xlsx("raw_data/tea_bag_labels_2025.xlsb")

teabag.data = read_xlsx("raw_data/tea_bag_labels_2025.xlsb", rows = c(2:162), cols = 7:15, na.strings = c("NA", "")) |>
  add_column(set = "data")

# teabag.handlingLoss = read_xlsx("raw_data/tea_bag_labels_2025.xlsb", rows = c(2, 164:169), cols = 7:12) |>
#   mutate(set = "handling_loss",
#          treatment = as.character(treatment))
#
# teabag.HLbags = read_xlsx("raw_data/tea_bag_labels_2025.xlsb", rows = c(2, 171:200), cols = 7:12) |>
#   mutate(set = "HL_bags",
#          collar = as.character(collar),
#          'Post dry weight' = as.numeric(`Post dry weight`))

# Metadata about which collars are useable ----
teabag.flags = read_xlsx("raw_data/teabags_labels+collars+treatment_noweight.xlsx", start_row = 2, start_col = 2)

teabag = teabag.data |>
  # Rename columns with space
  janitor::clean_names() |>
  # Add in flags
  full_join(teabag.flags) |>
  # Remove the duds
  filter(`Usable y/n` == "y") |>
  # Expand collars into treatments
  separate(collar, into = c("collar_nr", "habitat", "site_abbrv"), remove = FALSE) |>
  # Fix AQ
  mutate(understory = case_when(
    str_detect(site, "Aqueduct") ~ site_abbrv,
    TRUE ~ NA
  ),
  site_abbrv = case_when(
    str_detect(site, "Aqueduct") ~ "aq",
    str_detect(site, "Spencers") ~ "sp",
    str_detect(site, "Guthega") ~ "gu",
    str_detect(site, "Pipers") ~ "pi",
    str_detect(site, "2k") ~ "2k",
    TRUE ~ site_abbrv
  )) |>
  # Calculate final weight
  mutate(final_weight = as.numeric(final_weighed_without_tag),
         final_weight = case_when(
           final_weight == 921 ~ 0.921,
           TRUE ~ final_weight
         ),
    loss = post_dry_weight - final_weight) |>
  # Remove ones eaten by wombat
  filter(!is.na(final_weight)) |>
  # Add in dates
  mutate(burial_date = ymd("2024-11-16"),
         recover_date = ymd("2025-02-04") # This needs changing for each site
         ) |>
  # Put in order
  mutate(site_abbrv = factor(site_abbrv, levels = c("sp", "aq", "pi", "2k", "gu"),
                             labels = c("Healthy", "Light", "Moderate", "Severe", "All trees dead")),
         treatment = factor(treatment, levels = c("GT", "R"),
                            labels = c("Green Tea", "Rooibos")),
         habitat = factor(habitat, levels = c("f", "o"), labels = c("Forested", "Open")))


# Visualise ----
## Weights -----
ggplot()

## Colour codes -----
ggplot(teabag |> filter(!is.na(site_abbrv)),
       aes(x = habitat, y = loss, colour = habitat, fill = habitat)) +
  # geom_boxplot(alpha = 0.5) +
  geom_point(inherit.aes = FALSE, aes(x = habitat, y = loss, colour = colour_code), size = 5) +
  scale_colour_manual(values = c("grey80", "purple", "red", "black")) +
  facet_grid(treatment~site_abbrv) +
  theme_bw()

# Habitat and site ----
ggplot(teabag |> filter(!is.na(site_abbrv)) |>
         filter(site_abbrv %in% c("Healthy", "Light", "Moderate")),
       aes(x = interaction(habitat), y = loss, colour = habitat, fill = habitat)) +
  geom_violin() +
  geom_boxplot(alpha = 0, colour = "grey80") +
  # geom_jitter(position = position_jitterdodge()) +
  scale_fill_manual(values = c("forestgreen", "skyblue3")) +
  scale_colour_manual(values = c("forestgreen", "skyblue3")) +
  facet_grid(treatment~site_abbrv, scales = "free_y") +
  labs(x = "Habitat", y = "Mass loss (g)") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(paste0("outputs/", Sys.Date(), "_Teabags_SiteXhabitat.png"))

library(ggh4x)
ggplot(teabag |> filter(!is.na(site_abbrv)) |>
         filter(site_abbrv %in% c("Healthy", "Light", "Moderate")) |>
         filter(treatment == "Rooibos"),
       aes(x = habitat, y = loss, colour = habitat, fill = habitat)) +
  # geom_violin() +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(), size = 3) +
  scale_fill_manual(values = c("forestgreen", "skyblue3")) +
  scale_colour_manual(values = c("forestgreen", "skyblue3")) +
  facet_grid(~site_abbrv, scales = "free_y") +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", y = "Mass loss (g)") +
  theme_classic() +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.title.y = ggtext::element_markdown(),
        text = element_text(size = 25))

ggsave(paste0("outputs/", Sys.Date(), "_Teabags_SiteXhabitat_Focal.png"),
       width = 14, height = 8, units = "in")


ggplot(teabag |> filter(!is.na(site_abbrv)),
       aes(x = habitat, y = loss, colour = treatment, fill = treatment)) +
  geom_violin() +
  geom_boxplot(alpha = 0, colour = "grey80") +
  scale_fill_manual(values = c("olivedrab4", "chocolate3")) +
  scale_colour_manual(values = c("olivedrab4", "chocolate3")) +
  facet_grid(treatment~site_abbrv, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(paste0("outputs/", Sys.Date(), "_Teabags_Flavour.png"))
