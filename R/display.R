#' display_table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function displays a data frame as a html table using the [reactable](https://glin.github.io/reactable/)
#' package. The top ten rows in the table are displayed with the options to click
#' through or display all the rows. The ability to search for table content and
#' sort columns is also available.
#'
#' @param data data frame, data to display
#' @param rows integer, number of rows to display with total rows as default
#'
#' @return html table to display
#' @export
#'
#' @examples
#'suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'    library(palmerpenguins)
#'    library(dplyr)
#'  })
#'})
#' # display top 5 heaviest penguins from each species on each island
#' heaviest_penguins <- penguins %>%
#'   select(species, island, body_mass_g) %>%
#'   group_by(species, island) %>%
#'   arrange(desc(body_mass_g)) %>%
#'   slice_head(n = 5) %>%
#'   ungroup()
#' display_table(heaviest_penguins)
display_table <- function(data, rows = nrow(data)) {
  reactable::reactable(data[1:rows, ],
                       fullWidth = FALSE,
                       searchable = TRUE,
                       compact = TRUE,
                       wrap = FALSE,
                       resizable = TRUE,
                       defaultColDef = reactable::colDef(align = "left"),
                       showPageSizeOptions = TRUE,
                       pageSizeOptions = c(10, nrow(data)),
                       class = "table")
}

