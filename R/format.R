#' extract_delimited_list
#'
#' This function returns the parent function of the function factory to extract
#' a delimited list from a data frame's column
#'
#' @param delimiter character, delimiter
#'
#' @return function, extracts the column as a delimited list
extract_delimited_list <- function(delimiter) {

  function(.df, column_name) {

    df_column <- .df %>%
      dplyr::select({{column_name}}) %>%
      dplyr::arrange({{column_name}}) %>%
      dplyr::nest_by() %>%
      dplyr::mutate(dplyr::across(data, function(x) {purrr::map_chr(x, paste0, collapse = delimiter)})) %>%
      dplyr::pull() %>%
      unname()
  }
}

#' extract_comma_delimited_list
#'
#' This function extracts a comma delimited list from a data frame's column
#' @param .df data frame, data frame containing the column to extract the delimited list
#' @param column_name character, name of column to extract the delimited list
#'
#' @return character, comma delimited list
#' @export
#'
#' @examples
#' suppressPackageStartupMessages({
#'  library(dplyr)
#'  library(purrr)
#'  library(palmerpenguins)
#' })
#' library(store)
#' # select top 5 heaviest penguins from each species on each island
#' heaviest_penguins <- penguins %>%
#'  select(species, island, body_mass_g) %>%
#'  group_by(species, island) %>%
#'  arrange(desc(body_mass_g)) %>%
#'  slice_head(n = 5) %>%
#'  ungroup()
#' heaviest_penguins
#'
#' # extract comma separated list of penguin weights for each species on each island
#' heaviest_penguins <- heaviest_penguins %>%
#'  group_nest(across(c(species:island)), .key = "penguins") %>%
#'  mutate(weight = map_chr(penguins, extract_comma_delimited_list, column_name = "body_mass_g")) %>%
#'  select(-penguins)
#' heaviest_penguins
extract_comma_delimited_list <- extract_delimited_list(delimiter = ", ")

#' extract_semicolon_delimited_list
#'
#' This function extracts a semi-colon delimited list from a data frame's column
#' @param .df data frame, data frame containing the column to extract the delimited list
#' @param column_name character, name of column to extract the delimited list
#'
#' @return character, semi-colon delimited list
#' @export
#'
#' @examples
#' suppressPackageStartupMessages({
#'  library(dplyr)
#'  library(purrr)
#'  library(palmerpenguins)
#' })
#' library(store)
#' # select top 5 heaviest penguins from each species on each island
#' heaviest_penguins <- penguins %>%
#'  select(species, island, body_mass_g) %>%
#'  group_by(species, island) %>%
#'  arrange(desc(body_mass_g)) %>%
#'  slice_head(n = 5) %>%
#'  ungroup()
#' heaviest_penguins
#'
#' # extract comma separated list of penguin weights for each species on each island
#' heaviest_penguins <- heaviest_penguins %>%
#'  group_nest(across(c(species:island)), .key = "penguins") %>%
#'  mutate(weight = map_chr(penguins, extract_semicolon_delimited_list, column_name = "body_mass_g")) %>%
#'  select(-penguins)
#' heaviest_penguins
extract_semicolon_delimited_list <- extract_delimited_list(delimiter = "; ")



