#' Add a new spreadsheet containing data and an image to an Excel workbook
#'
#' @description
#' This function adds a data frame and optional image to a new spreadsheet in an
#' Excel workbook.
#'
#' The text is left aligned with the first header row made bold. The widths of the
#' columns in the spreadsheet are also  automatically adjusted to fit the text and
#' the filter functionality is switched on.
#'
#' An image may be inserted to the right of the text with both the image's width
#' and height sizes adjustable by providing their sizes in inches in the function's
#' parameters.
#'
#' @family excel functions
#'
#' @param spreadsheet_name character string, name of spreadsheet to add
#' @param data data_frame, data to add to spreadsheet
#' @param image_path character string, path to image file if present, default is NA
#' @param image_width double, width of image in inches
#' @param image_height double, height of image in inches
#' @param filter logical, add filter to spreadsheet (default = TRUE) or not (FALSE)
#'
#' @return data_frame, the inputted data frame is returned invisibly
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
#'     library(stringr)
#'     library(ggplot2)
#'     library(here)
#'     library(lubridate)
#'     library(openxlsx)
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
#' ## data
#' mass_flipper_data <- penguins %>%
#'   select(species, flipper_length_mm, body_mass_g, )
#' mass_flipper_data
#'
#' ## graph
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
#'       color = "Penguin species",
#'        shape = "Penguin species") +
#'   theme(legend.position = c(0.2, 0.7),
#'         legend.background = element_rect(fill = "white", color = NA),
#'         plot.title.position = "plot",
#'         plot.caption = element_text(hjust = 0, face= "italic"),
#'         plot.caption.position = "plot")
#'
#' mass_flipper_graph
#'
#' ## save image
#' ggsave(filename = "output/images/palmerpenguin_graph.png",
#'        plot = mass_flipper_graph)
#'
#' ## export data and graph into spreadsheet
#' workbook_path <- str_glue("output/example_{today()}.xlsx")
#' if(!file_exists(workbook_path)) {
#'   workbook <- createWorkbook()
#' } else {
#'   loadWorkbook(workbook_path)
#' }
#' add_new_spreadsheet("palmerpenguins", mass_flipper_data,
#'                     image_path = "output/images/palmerpenguin_graph.png",
#'                     image_width = 6,
#'                     image_height = 6)
#' saveWorkbook(workbook, workbook_path, overwrite = TRUE)
#' }
add_new_spreadsheet <- function(spreadsheet_name, data, image_path = NA_character_,
                                image_width, image_height, filter = TRUE) {
  # create spreadsheet
  sheet <- openxlsx::addWorksheet(wb = workbook, sheetName = spreadsheet_name)
  column_number <- ncol(data)
  row_number <- nrow(data)

  # add data
  openxlsx::writeData(workbook, sheet, data)
  openxlsx::setColWidths(workbook, sheet, cols = 1:column_number, widths = "auto")
  header_style <- openxlsx::createStyle(textDecoration = "bold")
  cell_style <- openxlsx::createStyle(halign = "left")
  openxlsx::addStyle(workbook, sheet, header_style, rows = 1, cols = 1:column_number)
  openxlsx::addStyle(workbook, sheet, cell_style,
                     gridExpand = TRUE, cols = 1:column_number, rows = 1:row_number + 1)
  if(filter) {
    openxlsx::addFilter(workbook, sheet, rows = 1, cols = 1:column_number)
  }

  # add image
  if(!is.na(image_path)) {
    openxlsx::insertImage(workbook, sheet,
                          file = image_path,
                          width = image_width, height = image_height, units = "in",
                          startRow = 5, startCol = column_number + 2)
  }
  invisible(data)
}
