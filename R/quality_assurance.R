#' Compare datasets
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function compares two versions of a dataset returning the dataset with the added, removed
#' or changed rows identified, using the [daff package](https://github.com/edwindj/daff).
#' The compared dataset can then be exported into an Excel spreadsheet to quickly
#' identify where values have been changed using conditional formatting, on text
#' containing #
#'
#' An initial check is required before using the function to make sure the column
#' names between the two datasets are identical. This can be done quickly using the
#' compare function in the [waldo package](https://www.tidyverse.org/blog/2020/10/waldo/).
#'
#' @param old_dataset data frame, earlier dataset for comparison
#' @param new_dataset data frame, later dataset for comparison
#'
#' @return data frame, compared dataset with new, removed or updated rows highlighted with #
#' @export
compare_datasets <- function(old_dataset, new_dataset) {

  # create temporary file path
  temp_file <- tempfile(pattern = "", fileext = ".csv")

  # compare datasets and export differences file
  daff::diff_data(old_dataset, new_dataset,
                  show_unchanged = TRUE,
                  show_unchanged_columns = TRUE) %>%
    daff::write_diff(temp_file)

  # format differences file
  difference_dataset <- readr::read_csv(temp_file, col_types = cols(X1 = col_skip()), skip = 1) %>%
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

