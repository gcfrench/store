#' @title
#' Add a new spreadsheet containing data and an image to an Excel workbook
#'
#' @description
#' This function adds a data frame and optional image to a new spreadsheet in an
#' Excel workbook.
#'
#' @details
#' The text is left aligned with the first header row made bold. The widths of the
#' columns in the spreadsheet are also automatically adjusted to fit the text and
#' the filter functionality is switched on.
#'
#' An image may be inserted to the right of the text with both the image's width
#' and height sizes adjustable by providing their sizes in inches in the function's
#' parameters.
#'
#' @family excel functions
#'
#' @param spreadsheet_name The name of the spreadsheet to which the data and optionally an image will be added.
#' @param data The name of the data frame containing the data to add to the spreadsheet.
#' @param image_path The path name for the image to be optionally added to the spreadsheet.
#' @param image_width The width of image in inches to be optionally added to the spreadsheet.
#' @param image_height The height of image in inches to be optionally added to the spreadsheet.
#' @param filter Should a filter be added to the data (default TRUE) in the spreadsheet or not (FALSE).
#'
#' @return The data frame is returned invisibly so that the function can be used in a piped workflow.
#'
#' @export
#'
#' @example man/examples/penguins_mass_flipper_plot.R
#'
#' @examples
#' \dontrun{
#' ## export data and graph into spreadsheet
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(fs)
#'     library(stringr)
#'     library(here)
#'     library(lubridate)
#'     library(openxlsx)
#'   })
#' })
#'
#' workbook_path <- path(tempdir(), str_glue("example_{today()}.xlsx"))
#' if(!file_exists(workbook_path)) {
#'   workbook <- createWorkbook()
#' } else {
#'   loadWorkbook(workbook_path)
#' }
#' add_new_spreadsheet(spreadsheet_name = "palmerpenguins",
#'                     data = penguins_mass_flipper,
#'                     image_path = path(tempdir(), "figures", "penguins_mass_flipper_plot.png"),
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
