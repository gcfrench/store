# Requires
## penguins_mass_flipper_plot.R example

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


#' @title
#' Check parquet file compression
#'
#' @description
#' This function checks the amount of compression attained when exporting a dataset using
#' the Apache parquet file format, by the [arrow package](https://arrow.apache.org/docs/r/).
#' It compares parquet file sizes for a dataset, exported uncompressed and compressed using different available
#' compression types, with the equivalent dataset's file size exported as a csv file.
#'
#' @param .data dataset to be checked for amount of compression using parquet format.
#' @param compression_type parquet compression types to check, defaults to all compression
#' types ("snappy", "gzip", "brotli", "zstd", "lz4", "lzo") unless one or more specific
#' compression types are supplied as a vector.
#'
#' @return message giving each file size and compression percentage
#' @export
#'
#' @examples
#' data(penguins, package = "palmerpenguins")
#' check_parquet_file_compression(penguins)
check_parquet_file_compression <- function(.data,
                                           compression_type =  c("snappy", "gzip", "brotli", "zstd", "lz4", "lzo")) {

  # create parquet file function
  create_parquet_file <- function(compression_type) {

    path_parquet <- fs::path(tempdir(),
                             glue::glue("{dataset_name}_{compression_type}.parquet"))

    if(arrow::codec_is_available(compression_type)) {
      arrow::write_parquet(.data, path_parquet, compression = compression_type)
      file_size_parquet <- as.numeric(fs::file_info(path_parquet)[, "size"])
      file_size_compression <- as.numeric((file_size_csv - file_size_parquet) / file_size_csv)
      message(glue::glue("{compression_type} compressed parquet file: {scales::percent(file_size_compression, accuracy = 0.01)} ({scales::comma(file_size_parquet, accuracy = NULL)} bytes)"))
    } else {
      message(glue::glue("{compression_type} compressed parquet file is not available on this machine"))
    }

  }

  # dataset name
  dataset_name <- deparse(substitute(.data))

  # csv file
  path_csv <- fs::path(tempdir(), glue::glue("{dataset_name}_csv.csv"))
  readr::write_csv(.data, path_csv)
  file_size_csv <- as.numeric(fs::file_info(path_csv)[, "size"])
  message(glue::glue("csv file size = {scales::comma(file_size_csv, accuracy = NULL)} bytes"))

  # uncompressed parquet file
  path_parquet <- fs::path(tempdir(), glue::glue("{dataset_name}_uncompressed.parquet"))
  arrow::write_parquet(.data, path_parquet, compression = "uncompressed")
  file_size_parquet <- as.numeric(fs::file_info(path_parquet)[, "size"])
  file_size_compression <- as.numeric((file_size_csv - file_size_parquet) / file_size_csv)
  message(glue::glue("uncompressed parquet file: {scales::percent(file_size_compression, accuracy = 0.01)} ({scales::comma(file_size_parquet, accuracy = NULL)} bytes)"))

  # compressed parquet files
  purrr::walk(compression_type, create_parquet_file)
}
