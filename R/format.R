#' @title
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

    function(.df, column_name, sort = FALSE, unique_list = FALSE, ...) {

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
        glue::glue_collapse({{column_name}}, sep = delimiter, ...) %>%
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
#' excludes NAs, optionally sorts the character string and adds text as a delimiter
#' to the last two items
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
#' @param sort logical, whether the list is sorted (TRUE) or not (default FALSE)
#' @param unique_list logical, whether to remove the duplicated elements (TRUE) or not (default FALSE)
#' @param last, character, optional string used to separate the last two items
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

#' @title
#' Extract a semi-colon delimited list of items
#'
#' This function extracts a semi-colon list from a data frame's column. It
#' excludes NAs, optionally sorts the character string and adds text as a delimiter
#' to the last two items
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
#' @param sort logical, whether the list is sorted (TRUE) or not (default FALSE)
#' @param unique_list logical, whether to remove the duplicated elements (TRUE) or not (default FALSE)
#' @param last, character, optional string used to separate the last two items
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

#' @title
#' Extract a space delimited list of items
#'
#' This function extracts a space delimited list from a data frame's column. It
#' excludes NAs, optionally sorts the character string and adds text as a delimiter
#' to the last two items
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
#' @param sort logical, whether the list is sorted (TRUE) or not (default FALSE)
#' @param unique_list logical, whether to remove the duplicated elements (TRUE) or not (default FALSE)
#' @param last, character, optional string used to separate the last two items
#'
#' @return character, space delimited list
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
#'  mutate(weight = map_chr(penguins, extract_space_delimited_list, column_name = "body_mass_g")) %>%
#'  select(-penguins)
#' heaviest_penguins
extract_space_delimited_list <- extract_delimited_list(delimiter = " ")



#' @title
#' Extract a paragraph delimited list of items
#'
#' This function extracts a paragraph delimited list from a data frame's column,
#' inserting two new lines and carriage returns. It excludes NAs, optionally sorts
#' the character string and adds text as a delimiter to the last two items
#'
#' @description
#' It is an example of the use of a [function factory](https://adv-r.hadley.nz/function-factories.html),
#' [tidy evaluation](https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/)
#' and purrr's [map function](https://purrr.tidyverse.org/reference/map.html).
#' The function may be called on a [nested data frame](https://cran.r-project.org/web/packages/tidyr/vignettes/nest.html)
#' to extract the paragraph delimited list
#'
#' @family delimited functions
#'
#' @param .df data frame, data frame containing the column to extract the delimited list
#' @param column_name character, name of column to extract the delimited list
#' @param sort logical, whether the list is sorted (TRUE) or not (default FALSE)
#' @param unique_list logical, whether to remove the duplicated elements (TRUE) or not (default FALSE)
#' @param last, character, optional string used to separate the last two items
#'
#' @return character, paragraph delimited list
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
#' # extract paragraph separated list of penguin weights for each species on each island
#' heaviest_penguins <- heaviest_penguins %>%
#'  group_nest(across(c(species:island)), .key = "penguins") %>%
#'  mutate(weight = map_chr(penguins, extract_paragraph_delimited_list, column_name = "body_mass_g")) %>%
#'  select(-penguins)
#' heaviest_penguins
extract_paragraph_delimited_list <- extract_delimited_list(delimiter = "\r\n\r\n")

#' @title
#' Add a tibble to a list
#'
#' @description
#' This function prepends tibbles to a list, creating the list
#' in the environment calling this function if not already present
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



