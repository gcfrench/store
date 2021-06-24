#' @title
#' Display a dataset as a table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function displays a data frame as a html table using the [reactable](https://glin.github.io/reactable/)
#' package. The top ten rows in the table are displayed with the options to click
#' through or display all the rows. The ability to search for table content and
#' sort columns is also available.
#'
#' @param data A data frame containing the data to display.
#' @param rows Number of rows to display as an integer with total row number as the default.
#'
#' @return A html table displaying the first page of the data limited to the specified number
#' of rows.
#'
#' @export
#'
#' @example man/examples/heaviest_penguins.R
#'
#' @examples
#' \dontrun{
#' # display table of penguin weights for each species on each island
#' display_table(heaviest_penguins)
#' }
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

#' @title
#' Add a shadow to an image
#'
#' @description
#' This function adds a border shadow to the image, archiving the original image
#' in a separate archive sub-directory.
#'
#' @details
#' The example batch runs the function on a graph from the
#' [palmerpenguins package](https://allisonhorst.github.io/palmerpenguins/articles/examples.html)
#' and is an example of parallelization using the [future](https://github.com/HenrikBengtsson/future)
#' and [furrr](https://davisvaughan.github.io/furrr/) packages.
#'
#' @section Figures:
#' \if{html}{\figure{penguins_mass_flipper_plot_shadow.png}{options: width=80\%}}
#'
#' @family image manipulation functions
#'
#' @param path_image The character string of the image path name.
#'
#' @return The path name of image returned invisibly so that the function can be
#' used in a piped workflow.
#'
#' @export
#'
#' @example man/examples/penguins_mass_flipper_plot.R
#'
#' @examples
#' # add shadows to graph image
#' suppressPackageStartupMessages({
#'    suppressWarnings({
#'       library(fs)
#'       library(future)
#'       library(furrr)
#'    })
#' })
#'
#' plan(multisession)
#' path(tempdir(), "figures") %>% {
#'    suppressMessages({dir_ls(., glob = "*.png")})} %>%
#'    future_walk(add_image_shadow,
#'               .options = furrr_options(seed = TRUE),
#'               .progress = TRUE)
#'
#' # move figures from temporary directory
#' suppressPackageStartupMessages({
#'    suppressWarnings({
#'       library(fs)
#'       library(here)
#'    })
#' })
#'
#' file_move(path(tempdir(), "figures", "penguins_mass_flipper_plot.png"),
#'           here("man", "figures", "penguins_mass_flipper_plot_shadow.png"))
add_image_shadow <- function(path_image) {

   # create archive directory if does not exist
   dir_archive <- fs::path(fs::path_dir(path_image), "archive")
   if(!fs::dir_exists(dir_archive)){
      fs::dir_create(dir_archive)
   }

   # move original image to archive directory
   path_archive <- fs::path(dir_archive, fs::path_file(path_image))
   fs::file_move(path_image, path_archive)

   # save image with shadow in original directory
   magick::image_read(path_archive) %>%
      magick::image_border(geometry = "1x1") %>%
      magick::image_shadow() %>%
      magick::image_write(path_image)

   # return path to reduced image
   invisible(path_image)
}

#' @title
#' Reduce the size of an image
#'
#' @description
#' This function reduces the size of an image, archiving the original image in a
#' separate archive sub-directory.
#'
#' @inherit add_image_shadow return details
#'
#' @section Figures:
#' \if{html}{\figure{penguins_mass_flipper_plot_reduced.png}}
#'
#' @seealso [Jumping rivers](https://www.jumpingrivers.com/) has written a series of
#' four blogs for [displaying images on web pages](https://www.jumpingrivers.com/blog/knitr-include-graphics-external/).
#'
#' @family image manipulation functions
#'
#' @inheritParams add_image_shadow
#' @param image_size Percentage size reduction with 50% as the default.
#'
#' @inherit add_image_shadow return return
#'
#' @export
#'
#' @example man/examples/penguins_mass_flipper_plot.R
#'
#' @examples
#' # batch reduce images
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(fs)
#'     library(future)
#'     library(furrr)
#'   })
#' })
#'
#' plan(multisession)
#' path(tempdir(), "figures") %>% {
#'   suppressMessages({dir_ls(., glob = "*.png")})} %>%
#'   future_walk(reduce_image_size,
#'               image_size = "50%",
#'               .options = furrr_options(seed = TRUE),
#'               .progress = TRUE)
#'
#' # move figures from temporary directory
#' suppressPackageStartupMessages({
#'    suppressWarnings({
#'       library(fs)
#'       library(here)
#'    })
#' })
#'
#' file_move(path(tempdir(), "figures", "penguins_mass_flipper_plot.png"),
#'           here("man", "figures", "penguins_mass_flipper_plot_reduced.png"))
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

