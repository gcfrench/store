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
#' @param data A data frame containing the data to display
#' @param rows Number of rows to display as an integer with total row number as the default
#'
#' @return A html table displaying the first page of the data limited to the specified number
#' of rows
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
#' in a separate archive sub-directory
#'
#' @seealso
#' The example batch runs the function on a graph from the
#' [palmerpenguins package](https://allisonhorst.github.io/palmerpenguins/articles/examples.html)
#' and is an example of parallelization using the [future](https://github.com/HenrikBengtsson/future)
#' and [furrr](https://davisvaughan.github.io/furrr/) packages
#'
#' @family image manipulation functions
#'
#' @param path_image The character string of the image path name
#'
#' @return The path name of image returned invisibly so that the function can be
#' used in a piped workflow
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
#' # copy shadow graph to man directory
#' suppressPackageStartupMessages({
#'    suppressWarnings({
#'       library(fs)
#'       library(here)
#'    })
#' })
#'
#' file_copy(path(tempdir(), "figures", "penguins_mass_flipper_plot.png"),
#'           here("man", "figures", "penguins_mass_flipper_plot.png"),
#'           overwrite = TRUE)
#'
#' @section Figures:
#' ![Penguins mass v flipper plot](penguins_mass_flipper_plot.png)
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
#' This function reduces the size of an image, archiving the
#' original image in a separate archive sub-directory
#'
#' @seealso
#' The example batch runs the function on a graph from the
#' [palmerpenguins package](https://allisonhorst.github.io/palmerpenguins/articles/examples.html)
#' and [palmer penguins image](https://allisonhorst.github.io/palmerpenguins/articles/art.html)
#' from Allison Horst's Art work. Artwork by @allison_horst and is an example of parallelization using
#' the [future](https://github.com/HenrikBengtsson/future) and [furrr](https://davisvaughan.github.io/furrr/)
#' packages
#'
#' @seealso [Jumping rivers](https://www.jumpingrivers.com/) has written a series of
#' four blogs for [displaying images on web pages](https://www.jumpingrivers.com/blog/knitr-include-graphics-external/).
#'
#' @family image manipulation functions
#'
#' @inheritParams add_image_shadow
#' @param image_size Percentage size reduction with 50% as the default
#'
#' @return The path name of image returned invisibly so that the function can be
#' used in a piped workflow
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(palmerpenguins)
#'     library(fs)
#'     library(dplyr)
#'     library(ggplot2)
#'     library(here)
#'     library(magick)
#'     library(future)
#'     library(furrr)
#'   })
#' })
#' # create output directory
#' i_am("example.Rmd")
#' if (!dir_exists("output")) {dir_create("output")}
#' if (!dir_exists("output/images")) {dir_create("output/images")}
#'
#' # example taken from palmerpenguins example analysis of mass vs. flipper length
#' # https://allisonhorst.github.io/palmerpenguins/articles/examples.html
#'
#' # data
#' mass_flipper_data <- penguins %>%
#'   select(species, flipper_length_mm, body_mass_g, )
#' mass_flipper_data
#'
#' # graph
#' mass_flipper_graph <- ggplot(data = mass_flipper_data,
#'                              aes(x = flipper_length_mm,
#'                                  y = body_mass_g)) +
#'   geom_point(aes(color = species,
#'                  shape = species),
#'              size = 3,
#'              alpha = 0.8) +
#'   theme_minimal() +
#'   scale_color_manual(values = c("darkorange","purple","cyan4")) +
#'   labs(title = "Penguin size, Palmer Station LTER",
#'        subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
#'        x = "Flipper length (mm)",
#'        y = "Body mass (g)",
#'        color = "Penguin species",
#'        shape = "Penguin species") +
#'   theme(legend.position = c(0.2, 0.7),
#'         legend.background = element_rect(fill = "white", color = NA),
#'         plot.title.position = "plot",
#'         plot.caption = element_text(hjust = 0, face= "italic"),
#'         plot.caption.position = "plot")
#'
#' # save graph
#' ggsave(filename = here("output", "images", "palmerpenguins_graph.png"),
#'        plot = mass_flipper_graph,
#'        type = "cairo-png",
#'        width = 8,
#'        height = 8,
#'        units = "in",
#'        dpi = 72)
#'
#' # graph image
#' mass_flipper_graph <- image_read(here("output", "images", "palmerpenguins_graph.png"))
#' mass_flipper_graph
#'
#' # graph information
#' image_info(mass_flipper_graph)
#'
#' # image taken from Allison Horst's Art for teaching with palmerpengiuns
#' # Citation: Artwork by @allison_horst
#' lter_penguins <- image_read("https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png")
#' lter_penguins
#'
#' # image information
#' image_info(lter_penguins)
#'
#' # save image
#' image_write(image = lter_penguins,
#'             path = here("output", "images", "palmerpenguins_image.png"))
#'
#' # batch reduce images
#' plan(multisession)
#' here("output/images") %>%
#'   dir_ls(., glob = "*.png") %>%
#'   future_map(reduce_image_size,
#'              image_size = "50%",
#'              .options = furrr_options(seed = TRUE),
#'              .progress = TRUE)
#'
#' # reduced graph
#' mass_flipper_graph_reduce <- image_read(here("output", "images", "palmerpenguins_graph.png"))
#' mass_flipper_graph_reduce
#'
#' # reduced graph information
#' image_info(mass_flipper_graph_reduce)
#'
#' # reduced image
#' lter_penguins_reduce <- image_read(here("output", "images", "palmerpenguins_image.png"))
#' lter_penguins_reduce
#'
#' # reduced image information
#' image_info(lter_penguins_reduce)
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

