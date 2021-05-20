#' @title
#' Compare two versions of a dataset
#'
#' @description
#'
#' This function compares two versions of a dataset returning the dataset with the added, removed
#' or changed rows identified, using the [daff package](https://github.com/edwindj/daff).
#' The compared dataset can then be exported into an Excel spreadsheet to quickly
#' identify where values have been changed using conditional formatting, on text
#' containing #
#'
#' An initial check should be performed prior to comparing versions to check that
#' the column names are identical and that there has not been any addition or removal
#' of columns between dataset versions, so that the dataset schema can be made the same
#' between versions if necessary. This check can be done using the compare function
#' in the [waldo package](https://www.tidyverse.org/blog/2020/10/waldo/).
#'
#' The resulting difference dataset may have a number of extra rows before the header
#' resulting in an error "object '@@' not found". The number of these rows to skip can be
#' adjusted with the skip_rows parameter
#'
#' @param old_dataset data frame, earlier dataset for comparison
#' @param new_dataset data frame, later dataset for comparison
#' @param skip_rows integer, number of rows to skip before header when importing
#' difference dataset (default = 0)
#'
#' @return data frame, compared dataset with new, removed or updated rows highlighted with #
#' @export
#'
#' @examples
#' suppressPackageStartupMessages({
#' library(store)
#' suppressWarnings({
#'   library(palmerpenguins)
#'   library(dplyr)
#'   library(purrr)
#'   library(waldo)
#'  })
#' })
#'
#' # select top 5 heaviest penguins from each species on each island
#' heaviest_penguins <- penguins %>%
#'   select(species, island, body_mass_g) %>%
#'   group_by(species, island) %>%
#'   arrange(desc(body_mass_g)) %>%
#'   slice_head(n = 5) %>%
#'   ungroup()
#'
#' # make two different versions
#'
#' ## unique identifier needed for comparison
#' heaviest_penguins <- heaviest_penguins %>%
#'   mutate(id = row_number()) %>%
#'   relocate(id)
#'
#' ## version 1
#' ### exclude Chinstrap penguins
#' heaviest_penguins_v1 <- heaviest_penguins %>%
#'   filter(species != "Chinstrap")
#'
#' ## version 2
#' ### exclude Gentoo penguins
#' ### convert body mass to kilograms
#' heaviest_penguins_v2 <- heaviest_penguins %>%
#'   filter(species != "Gentoo") %>%
#'   mutate(body_mass_g = body_mass_g / 1000) %>%
#'   rename(body_mass_kg = body_mass_g)
#'
#' # check columns and column names are identical between versions
#' waldo::compare(heaviest_penguins_v1, heaviest_penguins_v2)
#'
#' # make columns same between versions
#' heaviest_penguins_v1 <- heaviest_penguins_v1 %>%
#'   rename(body_mass = body_mass_g)
#'
#' heaviest_penguins_v2 <- heaviest_penguins_v2 %>%
#'   rename(body_mass = body_mass_kg)
#'
#' # compare versions of dataset
#' heaviest_penguins_compare <- suppressWarnings(compare_dataset_versions(heaviest_penguins_v1, heaviest_penguins_v2))
#' heaviest_penguins_compare
compare_dataset_versions <- function(old_dataset, new_dataset, skip_rows = 0) {

  # create temporary file path
  temp_file <- tempfile(pattern = "", fileext = ".csv")

  # compare datasets and export differences file
  daff::diff_data(old_dataset, new_dataset,
                  show_unchanged = TRUE,
                  show_unchanged_columns = TRUE) %>%
    daff::write_diff(temp_file)

  # format differences file
  difference_dataset <- readr::read_csv(temp_file,
                                        col_types = readr::cols(X1 = readr::col_skip()),
                                        skip = skip_rows) %>%
    dplyr::mutate(differences = case_when(
      `@@` == "->" ~ "# row changed",
      `@@` == "+++" ~ "# row added",
      `@@` == "---" ~ "# row removed",
      # `@@` == ":" ~ "# row_reordered",
      TRUE ~ NA_character_
    )) %>%
    dplyr::relocate(differences) %>%
    dplyr::select(-`@@`) %>%
    purrr::map_df(., stringr::str_remove, "NULL") %>%
    purrr::map_df(., stringr::str_replace, "->", " # ")

  # delete temporary file
  fs::file_delete(temp_file)

  # return
  return(difference_dataset)
}

