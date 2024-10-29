# Load packages
library(tidyverse)
library(tidylog)
library(Li6400Helper)

# If you don't already have `Li6400Helper`, unhash this code and run it
# install.packages("remotes")
# remotes::install_github("hrdawson/Li6400Helper", force = TRUE)

# Create a folder in your working directory (where your R project is stored) called `raw_data`.
# Inside `raw_data`, make a subfolder called `LI6400_SR`

# We need custom functions to read in all the files
# Set your cursor in the FIRST line of this bit of code and press run to store the function in your environment
Li6400Import_Data = function(file) {
  data1 = Li6400Import(file)
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

# Read in all the SR data
# For this next step, put your cursor onto the FIRST line (`temp_SR`) before clicking run
temp_SR_data = map_df(set_names(SR_file_list), function(file) {
  file %>%
    purrr::set_names() %>%
    map_df(~ Li6400Import_Data(file))
})

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

SR_noOddballs = temp_SR_wide %>%
  # Filter out non-obs
  drop_na(EFFLUX) %>%
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
  relocate(siteID, plotNr, Remarks, habitat_abbrv, X2, understory_abbrv, X3, X1, .after = Obs) |>
  # Code other useful variables
  mutate(habitat = case_when(
    siteID %in% c("gu", "sp", "2k") & habitat_abbrv == "f" | X2 == "f" ~ "Forested",
    siteID %in% c("gu", "sp", "2k") & habitat_abbrv == "o" | X2 %in% c("o", "g") ~ "Open",
    siteID == "pi" & (X2 == "g" | understory_abbrv == "g") ~ "Open",
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
  relocate(habitat, .after = X2) |> relocate(understory, .after = X3)

# Visualise the data
ggplot(SR_noOddballs |> filter(EFFLUX > 0) |>
         mutate(siteID = factor(siteID, levels = c("sp", "aq", "pi", "2k", "gu"))),
       aes(x = siteID, y = EFFLUX, colour = habitat, fill = habitat)) +
  geom_violin() +
  geom_boxplot(alpha = 0, colour = "grey80") +
  scale_y_log10() +
  facet_grid(~ habitat) +
  theme_bw()

# Let's deal with those now.

## FIRST work through this on an individual level ##
test = read.delim("raw_data/LI6400_SR_odd/2024.10.2_2k-f-1-sr", sep = "\t", header = FALSE)

test2 = test %>%
  # Assign IDs to each unique instance of 'OPEN 6.1.4'
  mutate(datasetID = case_when(
    V1 == "OPEN 6.1.4" ~ row_number(),
    TRUE ~ NA
  )) %>%
  # Downfill
  fill(datasetID, .direction = "down")

test2 %>%
  # Assign IDs to each unique instance of 'OPEN 6.1.4'
  mutate(datasetID = case_when(
    V1 == "OPEN 6.1.4" ~ row_number(),
    TRUE ~ NA
  )) %>%
  # Downfill
  fill(datasetID, .direction = "down") %>%
  # from https://luisdva.github.io/rstats/export-iteratively/
group_by(datasetID) %>% group_map(~.x, .keep = TRUE) %>%
  walk(~.x %>%  write_delim(file = paste0("raw_data/LI6400_SR/2024.10.2_2k-f-1-sr","-",
                                          unique(.x$datasetID),".tsv")))

test3 = Li6400Import("raw_data/LI6400_SR/2024.10.2_2k-f-1-sr-2549.tsv")

