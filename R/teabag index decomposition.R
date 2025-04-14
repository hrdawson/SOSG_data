# From Aud Halbritter's ThreeD project repo
# https://github.com/audhalbritter/Three-D/blob/master/R/functions/clean_decomposition.R

# Clean decomposition data

# clean_decomposition <- function(decomp_raw, decom_meta_raw, metaTurfID){

  # # mean weight of string & label
  # string_and_label <- mean(c(0.09177, 0.10385, 0.11177, 0.09002, 0.07527))
  #
  #
  #
  # meta <- decom_meta_raw %>%
  #   select(
  #     #IDs
  #     collar, treatment,
  #     # Burial dates
  #     spring_burialdate = tb.spring22.dateburied, spring_recoverdate = tb.spring22.dateretrieved,
  #     # Extra info
  #     comment) %>%
  #   mutate(burialdate = ymd(burialdate),
  #          recoverdate = ymd(recoverdate),
  #          )) %>%
  #   pivot_longer(cols = c(collar, burialdate, recoverdate),
  #                names_to = c("timing", ".value"),
  #                names_pattern = "(.*)_(.*)") %>%
  #   select(destSiteID:turfID, teabag_ID = ID, timing, burial_date = burialdate, recover_date = recoverdate)
  #
  #
  # decomposition <- decomp_raw %>%
  #   rename(pre_date_weighed = 'date_weighed...7',
  #          pre_weight_of = 'weight_of...8',
  #          post_date_weighed = 'date_weighed...11',
  #          post_weight_of = 'weight_of...12') |>
  #   mutate(comment_2 = tolower(comment_2),
  #          weight_comment = str_extract(comment_2, "no string"),
  #          comment_3 = if_else(str_detect(comment_2, "wrong number"), comment_2, NA_character_),
  #          comment_2 = case_when(str_detect(comment_2, "holes in the teabag") ~ "holes in the teabag",
  #                                str_detect(comment_2, "little|small") ~ "small hole",
  #                                str_detect(comment_2, "big") ~ "big hole",
  #                                str_detect(comment_2, "totally|empty|completely|completly|no teabag") ~ "totally destroyed",
  #                                TRUE ~ NA_character_)) %>%
  #   # fix burial depth
  #   mutate(tb_depth_cm = case_when(tb_depth_cm == "6,0/5,0" ~ "5.5",
  #                                  tb_depth_cm == "4,0/5,0" ~ "4.5",
  #                                  TRUE ~ tb_depth_cm),
  #          tb_depth_cm = as.numeric(tb_depth_cm)) %>%
  #   # remove 70 rows with NA for weight
  #   tidylog::filter(!is.na(post_burial_weight_g)) %>%
  #   # remove 58 completely destroyed teabags.
  #   tidylog::filter(!comment_2 %in% c("totally destroyed")) %>%
  #   # some NAs are introduced, is ok
  #   mutate(preburial_weight_g = as.numeric(preburial_weight_g),
  #          post_burial_weight_g = as.numeric(post_burial_weight_g),
  #          # adjust weight for teabags that have lost string and label
  #          post_burial_weight_g = if_else(!is.na(weight_comment),
  #                                         post_burial_weight_g - string_and_label,
  #                                         post_burial_weight_g)) %>%
  #   mutate(tea_type = recode(tea_type, "Rooibos" = "red", "Green Tea" = "green"))
  #
  #
  #
  # decomposition <- metaTurfID %>%
  #   left_join(meta, by = c("destSiteID", "destBlockID", "turfID")) %>%
  #   inner_join(decomposition, by = c("teabag_ID")) %>%
  #   # remove 29 tea bag without post burial weight
  #   tidylog::filter(!is.na(post_burial_weight_g)) %>%
  #   mutate(incubation_time = as.numeric(recover_date - burial_date),
  #          year = year(recover_date),
  #          timing = recode(timing, "fall" = "growing season", "spring" = "year")) |>
  #   select(year, origSiteID:Namount_kg_ha_y, teabag_ID, timing, tea_type, incubation_time, burial_depth_cm = tb_depth_cm, burial_date, preburial_weight_g, recover_date, post_burial_weight_g, comment_2)

# Format my data for Aud's function
decomp_clean = teabag |>
  mutate(tea_type = recode(treatment, "Rooibos" = "red", "Green Tea" = "green")) |>
  mutate(ID = paste0(site, "_", habitat, "_", collar_nr)) |>
  rename(preburial_weight_g = pre_dry_weight, post_burial_weight_g = final_weight, flag = 'Usable y/n')

calc_TBI_index <- function(decomp_clean){

  # List of variables
  ## tea_type
  ## preburial_weight_g_green
  ## post_burial_weight_g_green
  ## preburial_weight_g_red
  ## post_burial_weight_g_red
  ## recover_date
  ## burial_date
  ## flag

# Modified for Madame Flavour
  Hydrolysable_fraction_green = 0.746
  Hydrolysable_fraction_red = 0.699

  # Calculate tea bag index
  tea_bag_index <- decomp_clean |>
    # remove mislabelled collars
    filter(!ID %in% c("Pipers _Forested_1", "Pipers _Forested_8", "2k Grass_Open_6",
    "Aqueduct Grass_Forested_1", "Aqueduct Grass_Open_4")) |>
    select(collar, tea_type, preburial_weight_g, post_burial_weight_g, recover_date, burial_date) |>
    # split green and red tea into two columns
    pivot_wider(names_from = tea_type,
                values_from = c(preburial_weight_g, post_burial_weight_g),
                values_fill = NA) |>
    mutate(incubation_time = as.numeric(recover_date - burial_date),
           fraction_decomposed_green = 1 - post_burial_weight_g_green/preburial_weight_g_green,
           fraction_remaining_green = post_burial_weight_g_green/preburial_weight_g_green,
           fraction_remaining_red = post_burial_weight_g_red/preburial_weight_g_red,
           S = 1 - (fraction_decomposed_green / Hydrolysable_fraction_green),
           predicted_labile_fraction_red = Hydrolysable_fraction_red * (1 - S),
           k = log(predicted_labile_fraction_red / (fraction_remaining_red - (1 - predicted_labile_fraction_red))) / incubation_time)
    select(year:Namount_kg_ha_y, teabag_ID, timing, incubation_time, k, S, burial_date, recover_date, fraction_remaining_green, fraction_remaining_red, comment_2_red, comment_2_green) |>
    mutate(flag = if_else(grepl("big hole", comment_2_green)|grepl("big hole", comment_2_red), "hole in teabags", NA_character_)) |>
    select(-comment_2_green, -comment_2_red)

}

# # Check data
# tea_bag_index |>
#   #filter(grazing == "C") |>
#   ggplot(aes(x = Namount_kg_ha_y, y = fraction_remaining_green, colour = warming)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   scale_colour_manual(values = c("grey", "red")) +
#   facet_grid(origSiteID ~ grazing, scales = "free_y")
#
# # decomposition rate
# tea_bag_index |>
#   ggplot(aes(x = Namount_kg_ha_y, y = S, colour = warming)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   scale_colour_manual(values = c("grey", "red")) +
#   facet_grid(origSiteID ~ grazing)
