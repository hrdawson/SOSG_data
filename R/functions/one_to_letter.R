# Function to replace binaries with letters
# This function exists thanks to https://jonthegeek.com/2018/06/04/writing-custom-tidyverse-functions/
one_to_letter_stem = function(.data, ...) {
  .data |>
    pivot_longer(cols = ...,
                 names_to = "temp", values_to = "values") |>
    mutate(values = case_when(
      values == 1 ~ temp,
      TRUE ~ NA
    )) |>
    select(-c(temp)) |>
    group_by(plot, stem, tree) |>
    fill("values", .direction = "downup") |>
    distinct()
}

one_to_letter_plot = function(.data, ...) {
  .data |>
    pivot_longer(cols = ...,
                 names_to = "temp", values_to = "values") |>
    mutate(values = case_when(
      values == 1 ~ temp,
      TRUE ~ NA
    )) |>
    select(-c(temp)) |>
    group_by(plot) |>
    fill("values", .direction = "downup") |>
    distinct()
}

# Test code for the function
# subset = tree.data |>
#   slice_head(n = 10) |>
#   one_to_letter(c(e:n))
#
# subset = tree.data |>
#   slice_head(n = 10) |>
# pivot_longer(e:n,
#              names_to = "temp", values_to = "value") |>
#   mutate(value = case_when(
#     value == 1 ~ temp,
#     TRUE ~ NA
#   )) |>
#   rename(valuesTo = value) |>
#   select(-temp) |>
#   group_by(plot, tree, stem) |>
#   fill("valuesTo", .direction = "downup") |>
#   distinct()
