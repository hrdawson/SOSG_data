SR_noOddballs = temp_SR_wide %>%
  # Filter out non-obs
  drop_na(EFFLUX) %>%
  # write.csv("outputs/2024.10.29_LI6400_SR_AllCombined_Dormant.csv", row.names = FALSE)
  separate(remark, into = c("remark.timestamp", "plot_remarks"), sep = " ") %>%
  separate(plot_remarks, into = c("collar_remarks", "habitat_remarks", "understory_remarks", "X4"), sep = "-", remove = FALSE) %>%
  # Deciphering each column of the remarks
  mutate(collar_nr = case_when(
    str_detect(plot_remarks, "[:digit:]") ~ collar_remarks,
    TRUE ~ NA)) |>
  # Deciphering each part of the file name
  mutate(fileName = basename(File)) |>
  separate(fileName, into = c("fileDate", "plot_file"), sep = "_") |>
  separate(plot_file, into = c("siteID", "habitat_file", "understory_file", "rep", "fluxType"),
           sep = "-",
           remove = FALSE) |>
  # Filter to just the averaged efflux (final value)
  # filter(C2avg == 420) |>
  filter(Mode == 4) |>
  # Add in simple habitat controls
  mutate(habitat = case_when(
    habitat_file == habitat_remarks & habitat_file == "f" ~ "Forested",
    habitat_file == habitat_remarks & habitat_file == "o" ~ "Open",
  ),
  flag_quality = case_when(
    # SP
    siteID == "sp" & plot_file == "sp-o-g-01-sr" & Obs %in% c("16", "32", "47") ~ "discard", #bad run
    siteID == "sp" & Obs == 41 & fileDate == '2024.10.01' ~ "discard",
    siteID == "sp" & fileDate == "2024-12-04" & Obs %in% c("409", "420", "431") ~ "discard", # Moved
    siteID == "sp" & habitat_remarks == "moved" & fileDate == '2024.10.01' ~ "okay", # Collar moved but measurements were fine
    # AQ
    siteID == "aq" & fileDate == "2024.10.03" & plot_remarks == "1-f-s" & Obs %in% c("40", "50", "69") ~ "discard", # Moved
    siteID == "aq" & fileDate == "2025.01.08" & plot_file == "aq-f-o-s-g-sr" & Obs %in% c("258") ~ "discard",
    siteID == "aq" & fileDate == "2025.01.08" & plot_file == "aq-f-o-s-g-sr-2.0" & Obs %in% c("42") ~ "discard",
    siteID == "aq" & fileDate == "2024-12-06" & Obs == "141" ~ "discard",
    siteID == "aq" & plot_remarks == "aq-f-g-1-sr" & Obs == "91" ~ "discard", # Unknown treatment,
    #PI
    siteID == "pi" & fileDate == "2024-12-04" & is.na(habitat_remarks) ~ "double_check",
    siteID == "pi" & fileDate == "2024-10-04" & Obs %in% c("60", "73", "76") ~ "discard", # Equipment
    Smpls < 10 ~ "discard", # Too few reads for an accurate flux calculation
    # Counter statement
    TRUE ~ "okay"
  )) |>
  # Set campaign names
  mutate(campaign = case_when(
    HHMMSS %within% interval("2024-09-30", "2024-10-10") ~ "Dormant",
    HHMMSS %within% interval("2024-11-30", "2024-12-08") ~ "Greening up",
    HHMMSS %within% interval("2025-01-01", "2025-01-10") ~ "January",
    HHMMSS %within% interval("2025-02-01", "2025-02-10") ~ "Peak green",
    HHMMSS %within% interval("2025-03-31", "2025-04-05") ~ "Senescent"
  )) |>
  relocate(siteID, campaign, HHMMSS, plot_file, plot_remarks, fileDate,
           habitat_file, habitat_remarks, habitat,
           understory_file, understory_remarks,
           collar_remarks, .after = Obs) |>
  ## Add quality flags ##
  mutate(
    date = date(HHMMSS),
    flag_plot = case_when(
      # Dormant SP
      siteID == "sp" & plot_file == "sp-o-g-01-sr" & Obs %in% c("63", "79", "96") ~ "make_open_unspecified",
      is.na(habitat) & siteID == "sp" & fileDate == "2024.10.01" ~ "file_correct",
      # Dormant 2K
      is.na(habitat) & siteID == "2k" & fileDate == "2024.10.2" ~ "file_correct",
      # Dormant AQ
      is.na(habitat) & siteID == "aq" & HHMMSS %in% c("2024-10-03 15:11:21", "2024-10-03 15:12:19", "2024-10-03 15:13:28") ~ "understory_incorrect_use_grass",
      is.na(habitat) & siteID == "aq" & HHMMSS %in% c("2024-10-03 15:17:36", "2024-10-03 15:18:43", "2024-10-03 15:19:51") ~ "both_agree",
      siteID == "aq" & fileDate == "2024.10.03" & Obs %in% c("73", "83", "93") ~ "make_forest_unspecified",
      siteID == "aq" & fileDate == "2024.10.03" & plot_remarks == "3-o-g" ~ "make_open_unspecified",
      is.na(habitat) & siteID == "aq" & fileDate == "2024.10.03" & plot_remarks %in% c("1-f-g") ~ "both_agree",
      is.na(habitat) & siteID == "aq" & fileDate == "2024.10.03" ~ "remarks_correct",
      # Dormant PI
      is.na(habitat) & siteID == "pi" & fileDate == "2024-10-04" ~ "remarks_correct",
      is.na(habitat) & siteID == "pi" & fileDate == "2024-10-04" ~ "file_correct",
      # Dormant GU
      is.na(habitat) & siteID == "gu" & fileDate == "2024.10.04" & is.na(habitat_file) ~ "make_open",
      is.na(habitat) & siteID == "gu" & fileDate == "2024.10.02" ~ "file_correct",
      # Greening up SP
      is.na(habitat) & siteID == "sp" & fileDate == "2024-12-04" & plot_remarks != "08-0.2" ~ "remarks_correct",
      is.na(habitat) & siteID == "sp" & fileDate == "2024-12-04" & plot_remarks == "08-0.2" ~ "make_open",
      # Greening up PI
      is.na(habitat) & siteID == "pi" & fileDate == "2024-12-04" & is.na(habitat_remarks) ~ "make_forest",
      # Greening up AQ
      siteID == "aq" & plot_remarks == "control-aq-f" ~ "make_forest_unspecified",
      siteID == "aq" & plot_remarks == "aq-o-g-2-sr" & Obs == 145 ~ "make_forest_unspecified",
      siteID == "aq" & plot_remarks == "control-g-aq" ~ "make_open_unspecified",
      siteID == "aq" & fileDate == "2024-12-06" & Obs %in% c("10", "20", "30") ~ "remarks_correct",
      siteID == "aq" & plot_file == "aq-f-o-sr-3.0" & plot_remarks == "aq-o-g-3-sr" ~ "remarks_correct",
      is.na(habitat) & siteID == "aq" & fileDate == "2024-12-05" & Obs %in% c("40", "50", "60") ~ "make_open_shrub",
      is.na(habitat) & siteID == "aq" & fileDate == "2024-12-05" & Obs %in% c("70", "12", "23", "34") ~ "make_forest_grass",
      is.na(habitat) & siteID == "aq" & fileDate == "2024-12-05" & Obs %in% c("47", "61", "75") ~ "make_forest_shrub",
      is.na(habitat) & siteID == "aq" & fileDate == "2024-12-06" & Obs %in% c("101", "107", "125") ~ "make_open_grass",
      siteID == "aq" & plot_remarks == "aq-f-g-1-sr" & Obs %in% c("45", "59", "74") ~ "make_forest_shrub",
      siteID == "aq" & plot_remarks == "aq-1-o-g-sr" & Obs %in% c("135", "145", "155") ~ "make_open_shrub",
      siteID == "aq" & plot_remarks == "aq-o-s-2-sr" & Obs %in% c("85", "95", "105") ~ "make_open_shrub",
      siteID == "aq" & plot_remarks == "aq-o-g-2-sr" & Obs %in% c("115", "125", "135") ~ "make_open_grass",
      siteID == "aq" & plot_remarks == "aq-o-g-4-sr" & Obs %in% c("10", "20", "30") ~ "make_open_grass",
      is.na(habitat) & fileDate == "2024-12-05" & plot_file == "aq-f-o-s-g-sr" &
        plot_remarks %in% c("aq-o-g-4-sr", "aq-o-g-2-sr", "aq-o-s-2-sr",
                             "aq-f-g-2-sr", "aq-o-g-2-sr") ~ "remarks_correct",
      is.na(habitat) & siteID == "aq" & fileDate == "2024-12-05" & habitat_file == "s" ~ "make_forest_shrub",
      # is.na(habitat) & siteID == "aq" & fileDate == "2024-12-06" & Obs %in% c("274", "284", "294") ~ "make_open_grass", # Not sure what was intended ehre
      is.na(habitat) & siteID == "aq" & fileDate == "2025.01.08" & plot_file == "aq-f-o-s-g-sr" & Obs %in% c("42", "50", "55", "68") ~ "file_correct", #NOTE that the plot_remarks is truly correct
      is.na(habitat) & siteID == "aq" & fileDate == "2025.01.08" & plot_file == "aq-f-o-s-g-sr" & Obs %in% c("80", "88",
                                                           "93", "106", "116", "126", "137", "207",
                                                           "217", "227", "237", "247", "257", "305",
                                                           "316", "327") ~ "remarks_correct",
      is.na(habitat) & siteID == "aq" & fileDate == "2025.01.08" & plot_file == "aq-f-o-s-g-sr" & Obs %in% c("274", "284", "294") ~ "make_forest_grass",
      is.na(habitat) & siteID == "aq" & fileDate == "2025.01.08" & plot_file == "aq-f-o-s-g-sr-2.0" & Obs %in% c("21", "32", "132", "142", "152",
                                                                                              "162", "172", "178", "182") ~ "remarks_correct",
      is.na(habitat) & siteID == "aq" & fileDate == "2025.01.08" & plot_file == "aq-f-o-s-g-sr-2.0" & Obs %in% c("52", "62") ~ "make_open_shrub",
      is.na(habitat) & siteID == "aq" & fileDate == "2025.01.08" & plot_file == "aq-f-o-s-g-sr-2.0" & Obs %in% c("10") ~ "make_open_grass",
      # Peak green
      ## AQ
      habitat_file == "o" & habitat_remarks == "0" ~ "make_open",
      fileDate == "2025.02.05" & Obs %in% c("197", "208", "219") ~ "make_open_grass", # Checked against field notes
      fileDate == "2025.02.05" & Obs %in% c("229", "239", "249") ~ "make_open_shrub", # Checked against field notes
      fileDate == "2025.02.05" & plot_file == "aq-f-o-sr-2" & Obs %in% c("40") ~ "make_forest_grass", # Checked against field notes
      fileDate == "2025.02.05" & Obs %in% c("453", "463", "473") ~ "make_open_grass", # Checked against field notes (should be block 5)
      fileDate == "2025.02.05" & Obs %in% c("393", "403", "413") ~ "make_forest_shrub", # Checked against field notes
      siteID == "gu" & fileDate == "2025.02.04" & is.na(habitat_file) & habitat_remarks == "o" ~ "make_open",
      siteID == "2k" & fileDate == "2025.02.03" & habitat_file == "o" ~ "make_open",
      siteID == "aq" & fileDate == "2025.02.05" ~ "remarks_correct",
      siteID == "sp" & fileDate == "2025.02.06" & plot_file == "sp-f-sr" ~ "make_forest",
      # AQ March
      fileDate == "2025.03.07" & Obs %in% c("147", "161", "175") ~ "make_forest_grass", # Checked against field notes
      fileDate == "2025.03.07" & Obs %in% c("583", "595", "608") ~ "make_open_shrub", # Checked against field notes
      fileDate == "2025.03.07" & Obs %in% c("622", "635", "648") ~ "make_forest_shrub", # Checked against field notes
      fileDate == "2025.03.07" & Obs %in% c("658", "668", "678") ~ "make_forest_grass", # Checked against field notes
      is.na(habitat) & siteID == "aq" & fileDate == "2025.03.07" ~ "remarks_correct",
      siteID == "aq" & fileDate == "2025.03.07" & understory_remarks == "c" ~ "remove_understory",
      # Senscent
      fileDate == "2025.04.02" & plot_remarks == "6-f2k" ~ "file_correct", # based on the order of observations
      fileDate == "2025.04.01" & plot_remarks %in% c("2-o-s", "2-o-g", "3-o-c", "4-o-g", "5-o-g", "5-o-s") ~ "remarks_correct", # based off common error with remark/file labelling
      fileDate == "2025.04.03" & plot_remarks %in% c("7-o-sp") ~ "remarks_correct",
      fileDate == "2025.04.02" & plot_remarks %in% c("8-gu-f") ~ "file_correct",
      # Broad statements
      ## AQ
      is.na(habitat) & siteID == "aq" & understory_file != understory_remarks & habitat_file == habitat_remarks ~ "understory_disagree",
      ## Other sites
      is.na(habitat) & understory_file == understory_remarks & habitat_file != habitat_remarks ~ "habitat_disagree",
      is.na(habitat) & siteID %in% c("gu", "sp", "2k", "pi") & habitat_file != habitat_remarks ~ "habitat_disagree",
      is.na(habitat) & fileDate == "2024-12-05" & collar_remarks == "control" ~ "remove_understory",
      # Counter statement
    siteID %in% c("gu", "sp", "2k", "pi") & habitat_file == habitat_remarks ~ "okay",
    )
  ) |>
  # Code collar metadata
  mutate(
    habitat = case_when(
      flag_plot %in% c("make_open", "make_open_grass", "make_open_shrub", "make_open_unspecified") ~ "Open",
      flag_plot %in%  c("make_forest", "make_forest_shrub", "make_forest_unspecified", "make_forest_grass") ~ "Forested",
      flag_plot %in% c("both_agree", "remarks_correct", "okay", "understory_disagree") & habitat_remarks == "f" ~ "Forested",
      flag_plot %in% c("both_agree", "remarks_correct", "okay", "understory_disagree") & habitat_remarks %in% c("o", "g") ~ "Open",
      flag_plot %in% c("file_correct") & habitat_file == "f" ~ "Forested",
      flag_plot %in% c("file_correct") & habitat_file == "o" ~ "Open",
      fileDate == "2024-12-05" & plot_remarks == "control-aq-f" ~ "Forested",
      fileDate == "2024-12-05" & plot_remarks == "control-g-aq" ~ "Open",
      !is.na(habitat) ~ habitat,

    TRUE ~ NA
  ),
  understory = case_when(
    # Coerce unspecifieds
    siteID %in% c("2k", "sp", "pi", "gu") ~ "unspecified",
    siteID %in% c("aq") & collar_remarks == 3 ~ "unspecified",
    flag_plot %in% c("remove_understory", "make_forest_unspecified", "make_open_unspecified") ~ "unspecified",
    # Aqueduct
    siteID == "aq" & campaign == "January" & Obs %in% c("42", "50", "55", "68") ~ "Shrub",
    flag_plot == "remarks_correct" & understory_remarks == "g" ~ "Grass",
    flag_plot == "remarks_correct" & understory_remarks == "s" ~ "Shrub",
    flag_plot == "understory_incorrect_use_grass" ~ "Grass",
    flag_plot %in% c("both_agree", "habitat_disagree") & understory_remarks == "g" ~ "Grass",
    flag_plot %in% c("both_agree", "habitat_disagree") & understory_remarks == "s" ~ "Shrub",
    flag_plot %in% c("make_open_grass", "make_forest_grass") ~ "Grass",
    flag_plot %in%  c("make_forest_shrub", "make_open_shrub") ~ "Shrub",
    is.na(flag_plot) & understory_remarks == "g" ~ "Grass",
    is.na(flag_plot) & understory_remarks == "s" ~ "Shrub",
    TRUE ~ NA)) |>
  # Flag data quality
  mutate(flag_data = case_when(
    EFFLUX > 10 | EFFLUX < 0.7  ~ "efflux_suspect",
    RHirga. > 85 | RHirga. < 35 ~ "RH_suspect",
    Tair > 30 | Tair < 12 ~ "Tair_suspect",
    CO2S > 500  ~ "CO2_suspect",
    TRUE ~ "Okay"
  )) |>
  relocate(flag_plot, .after = habitat) |>
  relocate(understory, .after = understory_remarks) |>
  relocate(flag_quality)
