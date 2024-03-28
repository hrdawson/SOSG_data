library(rstatix)
# Which trees are missing from sequences? ----

missing.trees = tree.data.2024 |>
  # List distinct trees
  select(plot, treeNr) |>
  distinct() |>
  # Generate statistics
  group_by(plot) |>
  get_summary_stats(type = "common") |>
  select(plot, n, min, max) |>
  # Filter to just the plots missing trees
  filter(max != n)

data = missing.trees
plotNr = 26

find.trees = function(data, plotNr) {
  data1 = data |>
    filter(plot == plotNr) |>
    expand(min:max) |>
    rename(treeNr = 'min:max') |>
    mutate(plot = plotNr)

  data2 = data1 |>
    anti_join(tree.data.2024)
}

missing.trees.7 = missing.trees |> find.trees(7.2)
missing.trees.22 = missing.trees |> find.trees(22)
missing.trees.23 = missing.trees |> find.trees(23)
missing.trees.26 = missing.trees |> find.trees(26)
missing.trees.32 = missing.trees |> find.trees(32)
missing.trees.419 = missing.trees |> find.trees(419)

missing.trees.list = missing.trees.7 |>
  bind_rows(missing.trees.22, missing.trees.23, missing.trees.26, missing.trees.32, missing.trees.419)
