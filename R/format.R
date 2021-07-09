# Requires
## heaviest_penguins.R example
## test-format.R tests

#' @noRd
extract_delimited_list <- function(delimiter) {

    function(.df, column_name, sort = FALSE, unique_list = FALSE, last_delimiter = "", ...) {

      # extract required vector
      .vec <- .df %>%
        dplyr::select({{column_name}}) %>%
        tidyr::drop_na() %>%
        dplyr::pull()

      # check for empty vector
      if(rlang::is_empty(.vec)) {
        return(NA_character_)
      }

      # optionally remove duplicated elements
      if(unique_list) {
        .vec <- unique(.vec)
      }

      # optionally sort vector
      if(sort) {
        .vec <- .vec %>%
          stringr::str_sort()
      }

      # create delimited character string
      .vec <- .vec %>%
        glue::glue_collapse({{column_name}}, sep = delimiter, last = last_delimiter, ...) %>%
        vctrs::vec_cast(character())

      # add double quotes to ends of character string for paragraph delimiter
      if(stringr::str_detect(delimiter, "\r\n")) {
        .vec <- stringr::str_glue('"{.vec}"')
      }

      return(.vec)
  }
}

#' @title
#' Extract a comma delimited list of items
#'
#' @description
#' This function extracts a comma delimited list from a data frame's column. It
#' excludes NAs, optionally removes duplicate elements, sorts the character string
#' and adds text as a delimiter to the last two items.
#'
#' @details
#' It is an example of the use of a [function factory](https://adv-r.hadley.nz/function-factories.html),
#' [tidy evaluation](https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/)
#' and purrr's [map function](https://purrr.tidyverse.org/reference/map.html).
#' The function may be called on a [nested data frame](https://cran.r-project.org/web/packages/tidyr/vignettes/nest.html)
#' to extract the delimited list.
#'
#' @family delimited functions
#'
#' @param .df A data frame containing the column with the delimited list to extract.
#' @param column_name The name of the column containing the delimited list to extract.
#' @param sort Should the delimited list be sorted (TRUE) or not (default FALSE).
#' @param unique_list Should the delimited list contain duplicated elements (default FALSE) or not (TRUE).
#' @param last_delimiter An optional character string used to separate the last two items in the delimited list.
#'
#' @return The comma separated delimited list as a character string.
#'
#' @export
#'
#' @example man/examples/heaviest_penguins.R
#'
#' @examples
#' # extract comma separated list of penguin weights for each species on each island
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'    library(purrr)
#'   })
#' })
#'
#' heaviest_penguins %>%
#'  group_nest(across(c(species:island)), .key = "penguins") %>%
#'  mutate(weight = map_chr(penguins, extract_comma_delimited_list, column_name = "body_mass_g")) %>%
#'  select(-penguins)
extract_comma_delimited_list <- extract_delimited_list(delimiter = ", ")

#' @title
#' Extract a semi-colon delimited list of items
#'
#' @description
#' This function extracts a semi-colon delimited list from a data frame's column. It
#' excludes NAs, optionally removes duplicate elements, sorts the character string
#' and adds text as a delimiter to the last two items.
#'
#' @inherit extract_comma_delimited_list return details
#'
#' @family delimited functions
#'
#' @inheritParams extract_comma_delimited_list
#'
#' @return The semi-colon separated delimited list as a character string.
#'
#' @export
#'
#' @example man/examples/heaviest_penguins.R
#'
#' @examples
#' # extract comma separated list of penguin weights for each species on each island
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'    library(purrr)
#'   })
#' })
#'
#' heaviest_penguins %>%
#'  group_nest(across(c(species:island)), .key = "penguins") %>%
#'  mutate(weight = map_chr(penguins, extract_semicolon_delimited_list, column_name = "body_mass_g")) %>%
#'  select(-penguins)
extract_semicolon_delimited_list <- extract_delimited_list(delimiter = "; ")

#' @title
#' Extract a space delimited list of items
#'
#' @description
#' This function extracts a space delimited list from a data frame's column. It
#' excludes NAs, optionally removes duplicate elements, sorts the character string
#' and adds text as a delimiter to the last two items.
#'
#' @inherit extract_comma_delimited_list return details
#'
#' @family delimited functions
#'
#' @inheritParams extract_comma_delimited_list
#'
#' @return The space separated delimited list as a character string.
#'
#' @export
#'
#' @example man/examples/heaviest_penguins.R
#'
#' @examples
#' # extract comma separated list of penguin weights for each species on each island
#' library(purrr)
#' heaviest_penguins %>%
#'  group_nest(across(c(species:island)), .key = "penguins") %>%
#'  mutate(weight = map_chr(penguins, extract_space_delimited_list, column_name = "body_mass_g")) %>%
#'  select(-penguins)
extract_space_delimited_list <- extract_delimited_list(delimiter = " ")

#' @title
#' Extract a paragraph delimited list of items
#'
#' @description
#' This function extracts a paragraph delimited list from a data frame's column. It
#' excludes NAs, optionally removes duplicate elements, sorts the character string
#' and adds text as a delimiter to the last two items.
#'
#' @inherit extract_comma_delimited_list return details
#'
#' @family delimited functions
#'
#' @inheritParams extract_comma_delimited_list
#'
#' @return The paragraph separated delimited list as a character string.
#'
#' @export
#'
#' @example man/examples/heaviest_penguins.R
#'
#' @examples
#' # extract paragraph separated list of penguin weights for each species on each island
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'    library(purrr)
#'   })
#' })
#'
#' heaviest_penguins %>%
#'  group_nest(across(c(species:island)), .key = "penguins") %>%
#'  mutate(weight = map_chr(penguins, extract_paragraph_delimited_list, column_name = "body_mass_g")) %>%
#'  select(-penguins)
extract_paragraph_delimited_list <- extract_delimited_list(delimiter = "\r\n\r\n")

#' @title
#' Add a tibble to a list
#'
#' @description
#' This function prepends tibbles to a list, creating the list in the environment
#' calling this function if not already present.
#'
#' @details
#' Adding a tibble to a list is an example of [assigning a variable to an environment](http://adv-r.had.co.nz/Environments.html).
#'
#' @param .data The tibble to be added to the beginning of the list.
#' @param .name The name of the tibble to be added to the beginning of the list.
#'
#' @return The tibble is returned invisibly so that the function may be used within
#' a pipe workflow.
#'
#' @export
#'
#' @example man/examples/heaviest_penguins.R
#'
#' @examples
#' # prepend heaviest penguin species tibble to list
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'    library(forcats)
#'   })
#' })
#'
#' heaviest_penguins %>%
#'   filter(species == "Adelie") %>%
#'   mutate(species = fct_drop(species)) %>%
#'   add_tibble_to_list("adelie")
#'
#' heaviest_penguins %>%
#'   filter(species == "Chinstrap") %>%
#'   mutate(species = fct_drop(species)) %>%
#'   add_tibble_to_list("chinstrap")
#'
#' heaviest_penguins %>%
#'   filter(species == "Gentoo") %>%
#'   mutate(species = fct_drop(species)) %>%
#'   add_tibble_to_list("gentoo")
#' str(tibble_list, max.level = 1, list.len = 3)
#'
#' # convert list to tibble
#' bind_rows(tibble_list)
add_tibble_to_list <- function(.data, .name) {

  # environment calling this function
  parent_environment <- rlang::env_parent(env = rlang::caller_env(), n = 1)

  if(!exists("tibble_list", envir = parent_environment)) {
    assign("tibble_list", list(), envir = parent_environment)
  }
  tibble_list <- get("tibble_list", envir = parent_environment)
  .data <- .data %>%
    list() %>%
    purrr::set_names(.name) %>%
    purrr::prepend(tibble_list)
  assign("tibble_list", .data, envir = parent_environment)
  invisible(.data)
}



