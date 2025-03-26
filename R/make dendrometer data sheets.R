dendros = read.csv("raw_data/Dendrometers_from_spatial.csv") |>
  select(name, plotNickname, tree, stem, dbh, dendroNr, dendroType) |>
  relocate(dendroNr, .after = name) |>
  arrange(dendroNr) |>
  rename(GPS.point = name) |>
  nest(.by = plotNickname) |>
  pwalk(~write_csv(x = .y, path = paste0("outputs/", .x, ".csv") ) )
