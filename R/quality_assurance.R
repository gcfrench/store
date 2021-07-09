# Requires
## heaviest_penguins.R example

#' @title
#' Compare two versions of a dataset
#'
#' @description
#' This function compares two versions of a dataset returning the dataset with the added, removed
#' or changed rows identified, using the [daff package](https://github.com/edwindj/daff).
#' The compared dataset can then be exported into an Excel spreadsheet to quickly
#' identify where values have been changed using conditional formatting, on text
#' containing #.
#'
#' @details
#' An initial check should be performed prior to comparing versions to check that
#' the column names are identical and that there has not been any addition or removal
#' of columns between dataset versions, so that the dataset schema can be made the same
#' between versions if necessary. This check can be done using the compare function
#' in the [waldo package](https://www.tidyverse.org/blog/2020/10/waldo/).
#'
#' @param old_version The earlier version of the dataset as a data frame.
#' @param new_version The later version of the dataset as a data frame.
#'
#' @return The data frame with an additional difference column indicating new, removed or
#' updated rows highlighted with #.
#'
#' @export
#'
#' @example man/examples/heaviest_penguins.R
#'
#' @examples
# compare two versions of heaviest penguins dataset
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' ## each version will require an unique identifier
#' heaviest_penguins <- heaviest_penguins %>%
#'   mutate(id = row_number()) %>%
#'   relocate(id)
#'
#' ## old_version: exclude Chinstrap penguins
#' heaviest_penguins_old <- heaviest_penguins %>%
#'   filter(species != "Chinstrap")
#'
#' ## new_version: exclude Gentoo penguins and convert body mass to kilograms
#' heaviest_penguins_new <- heaviest_penguins %>%
#'   filter(species != "Gentoo") %>%
#'   mutate(body_mass_g = body_mass_g / 1000) %>%
#'   rename(body_mass_kg = body_mass_g)
#'
#' # check columns and column names are identical between versions
#' waldo::compare(heaviest_penguins_old, heaviest_penguins_new)
#'
#' # make columns same between versions
#' heaviest_penguins_old <- heaviest_penguins_old %>%
#'   rename(body_mass = body_mass_g)
#'
#' heaviest_penguins_new <- heaviest_penguins_new %>%
#'   rename(body_mass = body_mass_kg)
#'
#' # compare versions of dataset
#' suppressWarnings(compare_dataset_versions(heaviest_penguins_old, heaviest_penguins_new))
compare_dataset_versions <- function(old_version, new_version) {

 # create temporary file path
   temp_file <- tempfile(pattern = "", fileext = ".csv")

   # compare datasets versions and export differences file
   daff::diff_data(old_version, new_version,
                   show_unchanged = TRUE,
                   show_unchanged_columns = TRUE) %>%
     daff::write_diff(temp_file)

   # find row number with column names
   column_name_row_number <- readr::read_lines(temp_file, n_max = 10L) %>%
     stringr::str_which("@@")

   # format differences file
   difference_version <- readr::read_csv(temp_file,
                                         col_types = readr::cols(X1 = readr::col_skip()),
                                         skip = column_name_row_number - 1L) %>%
     dplyr::mutate(differences = dplyr::case_when(
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
   return(difference_version)
 }

