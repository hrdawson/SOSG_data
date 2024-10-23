# From audhalbritter/dataDocumentation
#' Make data dictionary
#' @description Function to make a data dictionary from any dataset.
#' @param data dataset for making data dictionary
#' @param description_table a table with additional information inlcuding variable name, description, unit and how variable was measured.
#' @param table_ID if duplicated variable names across datasets, this variable can specify the dataset.
#' @param keep_table_ID logical; argument keep table_ID or not.
#' The default is keep_table_ID = FALSE. If keep_table_ID = TRUE, table_ID argument is kept.
#'
#' @return a tibble
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr summarise across select mutate case_when left_join bind_rows inner_join filter everything all_of if_else
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble enframe
#' @importFrom lubridate is.Date is.POSIXct ymd
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#' @examples
#' data(description_table)
#' data(biomass)
#' data_dic <- make_data_dictionary(data = biomass,
#'                      description_table = description_table,
#'                      table_ID = "biomass",
#'                      keep_table_ID = FALSE)
#' @export

make_data_dictionary <- function(data, description_table, table_ID, keep_table_ID = FALSE){

  # use function get_range() to get range from characters, numeric variables and dates
  range <- get_range(data)

  # use get_class function to extract class from data
  class <- get_class(data)

  # combine range and class
  range_class <- class |>
    # join with range
    left_join(range, by = "Variable.name")  %>%
    mutate(TableID = {{table_ID}})

  # make dictionary
  dictionary <- bind_rows(
    # join general variables
    range_class %>%
      inner_join(description_table %>%
                   filter(is.na(.data$TableID)) %>%
                   select(-all_of("TableID")), by = "Variable.name"),
    # join special variables with same name but different meaning across datasets
    range_class %>%
      inner_join(description_table %>%
                   filter(.data$TableID == table_ID), by = c("Variable.name", "TableID"))
  ) %>%
    select(all_of(c("TableID", "Variable.name", "Description", "Variable type",
                    "Variable range or levels", "Units", "How.measured")))

  if(!keep_table_ID){
    dictionary <- dictionary %>%
      select(-all_of("TableID"))

  }

  dictionary

}

# get range from characters, numeric variables and dates
get_range <- function(data){

  # get range from each variable
  range <- data %>%
    as_tibble() %>%
    summarise(
      across(tidyselect::vars_select_helpers$where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
      across(tidyselect::vars_select_helpers$where(is.numeric), ~paste(round(min(., na.rm = TRUE), 3),round(max(., na.rm = TRUE), 3), sep = " - ")),
      across(tidyselect::vars_select_helpers$where(is.Date), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
      across(tidyselect::vars_select_helpers$where(is.POSIXct), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
    ) %>%

    # make long table
    pivot_longer(cols = everything(),
                 names_to = "Variable.name",
                 values_to = "Variable range or levels")

  range

}



# get class and make it into a tibble
get_class <- function(data){

  class <- map_dfr(data %>% as_tibble, ~enframe(class(.x)[1], value = "Variable type"),
                   .id = "Variable.name") %>%
    select(-all_of("name")) %>%

    # rename class
    mutate(`Variable type` = case_when(`Variable type` %in% c("character", "logical") ~ "categorical",
                                       `Variable type` %in% c("integer", "numeric") ~ "numeric",
                                       `Variable type` %in% c("Date") ~ "date",
                                       `Variable type` %in% c("POSIXct") ~ "date_time"))

  class

}
