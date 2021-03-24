#' extract_delimited_list
#'
#' @description
#' This function returns the parent function of the function factory to extract
#' a delimited list from a data frame's column. It excludes NAs but does not sort
#' the character string
#'
#' @param delimiter character, delimiter
#'
#' @return function, extracts the column as a delimited list
extract_delimited_list <- function(delimiter) {

  function(.df, column_name) {

    df_column <- .df %>%
      dplyr::select({{column_name}}) %>%
      tidyr::drop_na() %>%
      dplyr::nest_by() %>%
      dplyr::mutate(dplyr::across(data, function(x) {purrr::map_chr(x, paste0, collapse = delimiter)})) %>%
      dplyr::pull() %>%
      unname()
  }
}

#' extract_comma_delimited_list
#'
#' @description
#' This function extracts a comma delimited list from a data frame's column. It
#' excludes NAs but does not sort the character string
#'
#' It is an example of the use of a [function factory](https://adv-r.hadley.nz/function-factories.html),
#' [tidy evaluation](https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/)
#' and purrr's [map function](https://purrr.tidyverse.org/reference/map.html).
#' The function may be called on a [nested data frame](https://cran.r-project.org/web/packages/tidyr/vignettes/nest.html)
#' to extract the comma delimited list
#'
#' @family delimited functions
#'
#' @param .df data frame, data frame containing the column to extract the delimited list
#' @param column_name character, name of column to extract the delimited list
#'
#' @return character, comma delimited list
#' @export
#'
#' @examples
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'     library(dplyr)
#'     library(purrr)
#'   })
#' })
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
#' This function extracts a semi-colon delimited list from a data frame's column. It
#' excludes NAs but does not sort the character string
#'
#' @description
#' It is an example of the use of a [function factory](https://adv-r.hadley.nz/function-factories.html),
#' [tidy evaluation](https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/)
#' and purrr's [map function](https://purrr.tidyverse.org/reference/map.html).
#' The function may be called on a [nested data frame](https://cran.r-project.org/web/packages/tidyr/vignettes/nest.html)
#' to extract the semi-colon delimited list
#'
#' @family delimited functions
#'
#' @param .df data frame, data frame containing the column to extract the delimited list
#' @param column_name character, name of column to extract the delimited list
#'
#' @return character, semi-colon delimited list
#' @export
#'
#' @examples
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'     library(dplyr)
#'     library(purrr)
#'   })
#' })
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

#' add tibble to list
#'
#' @description
#' This function prepends tibbles to a list, creating the list
#' in the global environment if not already present
#'
#' It is an example of [assigning a variable to an environment](http://adv-r.had.co.nz/Environments.html)
#'
#' @param .data tibble, tibble to be added
#' @param .name character, name of tibble to be added
#'
#' @return tibble, original tibble is returned invisibly
#' @export
#'
#' @examples
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'     library(dplyr)
#'     library(forcats)
#'   })
#' })
#' # select top 3 heaviest penguins for each species
#' heaviest_penguins <- penguins %>%
#'   select(species, body_mass_g) %>%
#'   group_by(species) %>%
#'   arrange(desc(body_mass_g)) %>%
#'   slice_head(n = 3) %>%
#'   ungroup()
#' heaviest_penguins
#'
#' # prepend heaviest penguin species tibble to list
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
#' heaviest_penguins <- bind_rows(tibble_list)
#' heaviest_penguins
add_tibble_to_list <- function(.data, .name) {
  if(!exists("tibble_list", envir = globalenv())) {
    assign("tibble_list", list(), envir = globalenv())
  }
  tibble_list <- get("tibble_list", envir = globalenv())
  .data <- .data %>%
    list() %>%
    purrr::set_names(.name) %>%
    purrr::prepend(tibble_list)
  assign("tibble_list", .data, envir = globalenv())
  invisible(.data)
}



