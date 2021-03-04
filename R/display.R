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
#' @family tabular display
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

#' reduce_image_size
#'
#' @description
#' This function reduces the size of an image, archiving the
#' original image in a separate archive sub-directory
#'
#' @seealso The example batch runs the function and is an example of parallelization using
#' the [future](https://github.com/HenrikBengtsson/future) and [furrr](https://davisvaughan.github.io/furrr/)
#' packages
#'
#' @family image manipulation functions
#'
#' @param path_image character, path name of image
#' @param image_size percentage size reduction, default = 50%
#'
#' @return character, path name of image returned invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(fs)
#'     library(here)
#'     library(future)
#'     library(furrr)
#'   })
#' })
#'
#' i_am("example.Rmd")
#' plan(multiprocess)
#' here("figures") %>%
#'   dir_ls(., glob = "*.png") %>%
#'   future_map(reduce_image_size,
#'              image_size = "25%",
#'              .options = furrr_options(seed = TRUE),
#'              .progress = TRUE)
#'
#' }
reduce_image_size <- function(path_image, image_size = "50%") {

  # create archive directory if does not exist
  dir_archive <- fs::path(fs::path_dir(path_image), "archive")
  if(!fs::dir_exists(dir_archive)){
    fs::dir_create(dir_archive)
  }

  # move original image to archive directory
  path_archive <- fs::path(dir_archive, fs::path_file(path_image))
  fs::file_move(path_image, path_archive)

  # save reduced image in original directory
  magick::image_read(path_archive) %>%
    magick::image_scale(image_size) %>%
    magick::image_write(path_image)

  # return path to reduced image
  invisible(path_image)
}

