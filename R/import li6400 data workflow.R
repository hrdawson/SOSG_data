# Load packages
library(tidyverse)
library(tidylog)
# library(Li6400helper)

source("R/functions/Li6400Helper--Li6400Import_revised.R")

# If you don't already have `Li6400Helper`, unhash this code and run it
# install.packages("remotes")
# remotes::install_github("hrdawson/Li6400Helper", force = TRUE)

# Create a folder in your working directory (where your R project is stored) called `raw_data`.
# Inside `raw_data`, make a subfolder called `LI6400_SR`

# We need custom functions to read in all the files
# Set your cursor in the FIRST line of this bit of code and press run to store the function in your environment
source("R/functions/Li6400Helper--Li6400Import_revised.R")

Li6400Import_Data = function(file) {
  data1 = Li6400Import(file) # Use the custom script in `functions`
# data2 = data1$data %>%
  data2 = data1 %>%
  # This is a workaround
  mutate(File = file,
         uniqueID = paste0(File, row_number())) %>%
  pivot_longer(cols = FTime:Status, values_to = "value", names_to = "variable",
               values_transform = as.numeric)
}

# Do the same for this custom function (remember to put your cursor in the FIRST line)


# Make a list of all your LI6400 files
SR_file_list = dir(path = "raw_data/LI6400_SR/LI6400_files/subset",
                   full.names = TRUE, recursive = TRUE)

# temp_SR_data = purrr::map_dfr(SR_file_list, Li6400Import_Data)

# Read in all the SR data
# For this next step, put your cursor onto the FIRST line (`temp_SR`) before clicking run
temp_SR_data_raw = map_df(set_names(SR_file_list), function(file) {
  file %>%
    purrr::set_names() %>%
    map_df(~ Li6400Import_Data(file))
})

# Import the one file that the LI6400 only recorded as an xls

temp_SR_data_xls = read.csv("raw_data/LI6400_SR/field_session_3-3.02.2025/2025.02.06_pi-o-sr_.csv", skip = 9) |>
  mutate(remark = case_when(
    Obs == "Remark=" ~ str_remove_all(HHMMSS, "\""),
    TRUE ~ NA
  ),
  File = "raw_data/LI6400_SR/field_session_3-3.02.2025/2025.02.06_pi-o-sr_.csv",
  HHMMSS = lubridate::ymd_hms(paste0("2025-02-05 ", HHMMSS))) |>
  fill(remark, .direction = "down") |>
  filter(str_detect(Obs, '[1:9]')) |>
  pivot_longer(cols = FTime:Status, values_to = "value", names_to = "variable",
               values_transform = as.numeric) |>
  select(Obs, HHMMSS, remark, File, variable, value)

temp_SR_data = bind_rows(temp_SR_data_raw, temp_SR_data_xls)

#
# ### IMPORTANT ###
# # Read the warning messages in your console
# # Look for the messages that say "Use Li6400RemarkReshuffle with caution'
# # Each message should identify a filepath
# # These are files you need to move to a new folder so they don't interfere with the code.
# # (Don't worry, we'll revisit these files later.)
# # First, create the new folder
# dir.create("raw_data/LI6400_SR_odd")
#
# # Make a function to move each file
# # Place your cursor in the FIRST line to run
# Li6400Move = function(filepath) {
#   fs::file_move(filepath,
#                 "raw_data/LI6400_SR_odd")
# }
#
# # Then you can run the function for each file given in the warning messages
# # I've done the first one for you as an example
# Li6400Move("raw_data/LI6400_SR/LI6400_files/subset/2024.10.2_2k-f-1-sr")
#
# # Once you've moved the oddball files, go back
# # and rerun the code to make `SR_file_list` and `temp_SR_data`
#
# # Check the messages. Do you see the warning to use Li6400RemarksReshuffle with caution?
# # If yes, repeat the Li6400Move function.
# # If no, great! You're ready to move to the next step.

# Reformat the SR data
temp_SR_wide = temp_SR_data %>%
  pivot_wider(names_from = variable, values_from = value)

# # Read in all the SR remarks
# # For this next step, put your cursor onto the FIRST line (`temp_SR`) before clicking run
# Li6400Import_Remarks = function(file) {
#   data1 = Li6400Import(file) %>%
#     Li6400RemarkReshuffle() %>%
#     mutate(File = file) %>%
#     # Remove the time stamp from the remark
#     mutate(ForwardFilledRemarks = as.character(ForwardFilledRemarks),
#       Remarks = stringr::str_sub(ForwardFilledRemarks, start = 10)) %>%
#     # Also, sometimes the remarks did something wonky
#     mutate(Remarks = na_if(Remarks, "")) %>%
#     group_by(File) %>%
#     fill(Remarks, .direction = "down") %>%
#     ungroup() %>%
#     select(-ForwardFilledRemarks) %>%
#     rename(Obs = Row) %>%
#     distinct()
# }
#
# temp_SR_remarks = map_df(set_names(SR_file_list), function(file) {
#   file %>%
#     purrr::set_names() %>%
#     map_df(~ Li6400Import_Remarks(file))
# })

# Bring together the data and the remarks
# Make sure you have tidylog running for this step
library(tidylog)

# temp_SR_noOddballs = temp_SR_wide %>%
#   mutate(Obs = as.numeric(Obs)) %>%
#   left_join(temp_SR_remarks)

# Check the tidylog output
# Does the left_join table say 'with duplicates'?
# If it does, this is a red flag! You need to sleuth out what happened.
# If not, now you have functional data!
# Well, except for the instances that the LICOR restarted in the middle of a file...
# Before we deal with those, let's decipher the remarks so that they make sense

# temp_SR = read.csv("outputs/2024.10.29_LI6400_SR_AllCombined_Dormant.csv")

SR_noOddballs = temp_SR_wide %>%
  # Filter out non-obs
  drop_na(EFFLUX) |>
  # write.csv("outputs/2024.10.29_LI6400_SR_AllCombined_Dormant.csv", row.names = FALSE)
  separate(remark, into = c("remark.timestamp", "Remarks"), sep = " ") %>%
  separate(Remarks, into = c("X1", "X2", "X3"), remove = FALSE) %>%
  # Deciphering each column of the remarks
  mutate(collarNr = case_when(
    str_detect(X1, "[:digit:]") ~ X1,
    TRUE ~ NA)) |>
  # Deciphering each part of the file name
  mutate(fileName = basename(File)) |>
  separate(fileName, into = c("fileDate", "plotNr"), sep = "_") |>
  separate(plotNr, into = c("siteID", "habitat_abbrv", "understory_abbrv", "rep", "fluxType"),
           sep = "-",
           remove = FALSE) |>
  # Filter to just the averaged efflux (final value)
  # filter(C2avg == 420) |>
  # Code other useful variables
  mutate(habitat = case_when(
    siteID %in% c("gu", "sp", "2k") & habitat_abbrv == "f" | X2 == "f" ~ "Forested",
    siteID %in% c("gu", "sp", "2k") & habitat_abbrv == "o" | X2 %in% c("o", "g") ~ "Open",
    # siteID %in% c("pi") & (X2 == "g" | understory_abbrv == "g") ~ "Open",
    siteID == "sp" & habitat_abbrv == "g" ~ "Open",
    siteID == "pi" & X2 == "f" ~ "Forested",
    siteID == "gu" & fileDate == "2024.10.04" ~ "Open",
    siteID == "aq" & understory_abbrv == "f" ~ "Forested",
    siteID == "aq" & understory_abbrv == "o" ~ "Open",
    TRUE ~ NA
  ),
  understory = case_when(
    understory_abbrv == "g" | X3 == "g" ~ "Grass",
    understory_abbrv == "s" | X3 == "s" ~ "Shrub",
    understory_abbrv == "x" | X3 == "x" ~ "unspecified",
    siteID == "2k" ~ "unspecified",
    TRUE ~ NA)) |>
  rename(habitat_remarks = X2, understory_remarks = X3,
         habitat_file = habitat_abbrv, understory_file = understory_abbrv,
         plot_file = plotNr, plot_remarks = Remarks, collar_Nr = X1) |>
  relocate(siteID, plot_file, plot_remarks, fileDate,
           habitat_file, habitat_remarks, habitat,
           understory_file, understory_remarks, understory, collar_Nr, .after = Obs)

write.csv(SR_noOddballs, paste0("outputs/", Sys.Date(), "_LI6400_DataCombined.csv"), row.names = FALSE)

# Visualise the data
ggplot(SR_noOddballs |> filter(EFFLUX > 0) |> filter(flag_quality != "discard") |>
         # Filter to exclude major outliers
         filter(EFFLUX < 15) |>
         drop_na(campaign) |>
         filter(campaign != "January") |>
         mutate(siteID = factor(siteID, levels = c("sp", "aq", "pi", "2k", "gu"))),
       aes(x = campaign, y = EFFLUX, colour = habitat, fill = habitat)) +
  geom_violin() +
  geom_boxplot(alpha = 0, colour = "grey80") +
  # geom_point(position = position_jitterdodge()) +
  scale_fill_manual(values = c("forestgreen", "skyblue3")) +
  scale_colour_manual(values = c("forestgreen", "skyblue3")) +
  # scale_y_log10() +
  facet_grid(~siteID) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0("outputs/", Sys.Date(), "_EffluxBySiteAndHabitat_Seasonal.png"))

ggplot(SR_noOddballs |> filter(EFFLUX > 0) |> filter(flag_quality != "discard") |>
         drop_na(campaign) |>
         # Filter to exclude major outliers
         filter(EFFLUX < 15) |>
         filter(campaign != "January") |>
         mutate(siteID = factor(siteID, levels = c("sp", "aq", "pi", "2k", "gu"))),
       aes(x = interaction(habitat, campaign), y = EFFLUX, colour = habitat, fill = habitat)) +
  # geom_violin() +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_manual(values = c("forestgreen", "skyblue3")) +
  scale_colour_manual(values = c("forestgreen", "skyblue3")) +
  # scale_y_log10() +
  # facet_grid(~siteID) +
  labs(y = "CO<sub>2</sub> efflux (umol/m<sup>2</sup>/sec)", x = "") +
  scale_x_discrete(guide = "axis_nested") +
  theme_classic() +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.title.y = ggtext::element_markdown(),
        text = element_text(size = 25))

ggsave(paste0("outputs/", Sys.Date(), "_EffluxbySeason.png"),
       width = 14, height = 8, units = "in")

library(rmarkdown)

ggplot(SR_noOddballs |> filter(EFFLUX > 0) |> filter(flag_quality != "discard") |>
         # Filter to exclude major outliers
         filter(EFFLUX < 15) |>
         drop_na(campaign) |>
         filter(campaign != "January") |>
         filter(campaign == "Peak green") |>
         mutate(siteID = factor(siteID, levels = c("sp", "aq", "pi", "2k", "gu"),
                                labels = c("Healthy", "Light", "Moderate", "Severe", "All trees dead"))),
       aes(x = habitat, y = EFFLUX, colour = habitat, fill = habitat)) +
  # geom_violin() +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_manual(values = c("forestgreen", "skyblue3")) +
  scale_colour_manual(values = c("forestgreen", "skyblue3")) +
  # scale_y_log10() +
  facet_grid(~siteID) +
  labs(y = "CO<sub>2</sub> efflux (umol/m<sup>2</sup>/sec)", x = "") +
  theme_classic() +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.title.y = ggtext::element_markdown(),
        text = element_text(size = 25))

ggsave(paste0("outputs/", Sys.Date(), "_EffluxBySiteAndHabitat_PeakGreen.png"),
       width = 12, height = 8, units = "in")

ggplot(SR_noOddballs |> filter(EFFLUX > 0) |> filter(flag_quality != "discard") |>
         # Filter to exclude major outliers
         filter(EFFLUX < 15) |>
         drop_na(campaign) |>
         filter(campaign != "January") |>
         # filter(campaign == "Peak green") |>
         mutate(siteID = factor(siteID, levels = c("sp", "aq", "pi", "2k", "gu"),
                                labels = c("Healthy", "Light", "Moderate", "Severe", "All trees dead"))) |>
         filter(siteID %in% c("Healthy", "All trees dead")),
       aes(x = habitat, y = EFFLUX, colour = habitat, fill = habitat)) +
  # geom_violin() +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_manual(values = c("forestgreen", "skyblue3")) +
  scale_colour_manual(values = c("forestgreen", "skyblue3")) +
  # scale_y_log10() +
  facet_grid(~siteID) +
  labs(y = "CO<sub>2</sub> (umol/m<sup>2</sup>/sec)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = ggtext::element_markdown(),
        text = element_text(size = 20))

extreme_SR = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  filter(EFFLUX > 0) |>
  # Filter to exclude major outliers
  filter(EFFLUX < 15) |>
  filter(campaign == "Peak green") |>
  group_by(habitat, siteID) |>
  rstatix::get_summary_stats(EFFLUX) |>
  filter(siteID %in% c("gu", "sp"))

write.csv(extreme_SR, paste0("outputs/", Sys.Date(), "_SummaryStats.csv"), row.names = FALSE)

ggplot(SR_noOddballs |> filter(EFFLUX > 0) |> filter(flag_quality != "discard") |>
         drop_na(campaign) |>
         filter(campaign != "January") |>
         mutate(siteID = factor(siteID, levels = c("sp", "aq", "pi", "2k", "gu"),
                                labels = c("Healthy", "Light", "Moderate", "Severe", "All trees dead"))),
       aes(x = siteID, y = EFFLUX, colour = habitat, fill = habitat)) +
  geom_violin() +
  geom_boxplot(alpha = 0, colour = "grey80") +
  # geom_point(position = position_jitterdodge()) +
  scale_fill_manual(values = c("forestgreen", "skyblue3")) +
  scale_colour_manual(values = c("forestgreen", "skyblue3")) +
  # scale_y_log10() +
  # facet_grid(~siteID) +
  labs(y = "CO<sub>2</sub> (umol/m<sup>2</sup>/sec)", x = "") +
  theme_bw() +
  theme(axis.title.y = ggtext::element_markdown())

ggsave(paste0("outputs/", Sys.Date(), "_EffluxBySiteAndHabitat_NoFacet.png"))

# Find the oddballs

SR_missing_habitat = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  filter(is.na(habitat))

SR_missing_understory = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  filter(is.na(understory))

SR_mismatch_habitat = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  filter(habitat_file != habitat_remarks) |>
  arrange(HHMMSS, Obs)

SR_mismatch_understory = SR_noOddballs |>
  filter(understory_file != understory_remarks) |>
  relocate(understory_file, understory_remarks, understory, .after = plot_remarks) |>
  arrange(fileDate, Obs)

SR_EFFLUX = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  filter(EFFLUX > 10 | EFFLUX < 1) |>
  arrange(EFFLUX) |>
  relocate(EFFLUX, siteID, habitat, understory, .after = plot_remarks) |>
  select(-flag_quality)

hist(SR_noOddballs$H2OS)

ggplot(SR_noOddballs |> filter(flag_quality != "discard") |>
         mutate(siteID = factor(siteID, levels = c("sp", "aq", "pi", "2k", "gu")),
                flag_data = factor(flag_data, levels = c("Okay", "efflux_suspect", "RH_suspect", "Tair_suspect", "CO2_suspect"))),
       aes(x = siteID, y = EFFLUX, colour = flag_data)) +
  geom_jitter() +
  scale_colour_manual(values = c("grey70", "red", "orange", "yellow")) +
  # scale_y_log10() +
  # facet_grid(~ habitat) +
  theme_bw()

# Data by site and campaign
SR_duplicates = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  # filter(siteID != "aq") |>
  group_by(siteID, campaign, habitat) |>
  summarize(n = length(Obs))

SR_check = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  group_by(campaign, siteID, habitat) |>
  summarize(n = length(Obs)) |>
  pivot_wider(names_from = siteID, values_from = n)

SR.subset = function(site, campaignID){
  SR_noOddballs |>
    filter(flag_quality != "discard") |>
    filter(siteID == site) |>
    filter(campaign == campaignID) |>
    relocate(siteID, campaign, flag_quality, .after = collar_remarks) |>
    relocate(habitat, understory, flag_plot, .after = plot_remarks) |>
    arrange(HHMMSS)
}

SR_site_campaign = SR.subset("sp", "Senescent")

SR_sp_dormant = SR.subset("sp", "Dormant")

# Work with just Aqueduct ----
library(ggh4x)

AQ.dormant = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  filter(siteID == "aq") |>
  # just the final flux readout
  filter(Mode == 4) |>
  filter(understory != "unspecified")

ggplot(AQ.dormant |> filter(EFFLUX >= 0),
       aes(x = understory, y = EFFLUX, colour = understory, fill = understory)) +
  # geom_violin() +
  geom_boxplot(alpha = 0.7) +
  scale_colour_manual(values = c("gold2", "dodgerblue3")) +
  scale_fill_manual(values = c("gold2", "dodgerblue3")) +
  # scale_y_log10() +
  facet_grid(~habitat) +
  labs(x = "", y = "CO2 efflux") +
  scale_x_discrete(guide = "axis_nested") +
  theme_bw() +
  theme(text = element_text(size = 20))

# ggsave("outputs/2024.12.10_SoilResp.png")

# Habitat, dieback, and time, all in one go ----
SR_clean = SR_noOddballs |>
  filter(flag_quality != "discard") |>
  filter(flag_data != "efflux_suspect") |>
  # Leave off AQ extra sampling for now
  filter(campaign != "January") |>
  # Mutate the factors
  mutate(campaign = factor(campaign, levels = c("Dormant", "Greening up", "Peak green")),
         siteID = factor(siteID, levels = c("sp", "aq", "pi", "2k", "gu")))

ggplot(SR_clean,
       aes(x = campaign, y = EFFLUX, colour = siteID)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge()) +
  scale_colour_viridis_d(option = "C", direction = 1) +
  facet_grid(habitat ~.) +

  # scale_y_log10() +
  theme_bw()

ggsave("outputs/2025.02.10_Efflux_3campaigns.png")

ggplot(SR_clean,
       aes(x = siteID, y = EFFLUX, colour = campaign)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge()) +
  scale_colour_manual(values = c("gold", "lightgreen", "darkgreen")) +
  facet_grid(habitat ~.) +
  theme_bw()

ggsave("outputs/2025.02.10_Efflux_bySite.png")

