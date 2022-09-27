#' @title
#' Merge pdf documents
#'
#' @description
#' This function merges one or more pdf documents, saving the resulting merged
#' pdf document either to a provided path or default merged pdf name.
#'
#' @family pdf
#'
#' @param pdf_paths vector of pdf document paths in the order they are to be to merged,
#' default NULL to select the order of the pdf document paths.
#' @param saved_merged_pdf_path character string of merged pdf document path, default
#' NULL to select merged pdf document path.
#'
#' @return vector of pdf document paths to merge
#' @export
merge_pdf_documents <-function(pdf_paths = NULL,
                               saved_merged_pdf_path = NULL) {

  # option message
  select_pdf_document <- function(pdf_paths) {

    message("
1: Select pdf document to include in merged document
2: Select final pdf document to include in merged document")

    # select pdf document option number
    option_number <- as.integer(readline("Enter option number: "))

    # check option number exists
    assertthat::assert_that(option_number %in% c(1:2),
                            msg = "Selected option number does not exist, please try again...")

    # append selected pdf path
    pdf_paths <- append(pdf_paths,
                        choose.files(caption = "Please select pdf document to merge",
                                     multi = FALSE, filters = Filters[c("pdf"), ]))

    # append another selected pdf path if required
    if(option_number == 1) {
      select_pdf_document(pdf_paths)
    } else if (option_number == 2) {
      return(pdf_paths)
    }
  }

  # Select pdf documents to merge if not provided
  if(is.null(pdf_paths)) {
    message("
Select pdf documents to merge in order
--------------------------------------")
    pdf_paths <- select_pdf_document(pdf_paths)
  }

  # Select output directory to export merged document if not provided
  if(is.null(saved_merged_pdf_path)) {
    saved_merged_pdf_path = fs::path(choose.dir(caption = "Please select directory to save merged pdf document"),
                                     "merged.pdf")
  }

  # merge pdf documents
  pdftools::pdf_combine(input = pdf_paths,
                        output = saved_merged_pdf_path)

  # return pdf paths invisibly
  invisible(pdf_paths)
}

#' @title
#' Count pdf document pages
#'
#' @description
#' This function counts the number of pages in the pdf document
#'
#' @family pdf
#'
#' @param pdf_path path character string of pdf document
#'
#' @return integer of number of pages in pdf document
#' @export
count_pdf_document_pages <- function(pdf_path) {

  # check file exists
  if(!fs::file_exists(pdf_path)) {
    message(glue::glue("{pdf_path} does not exist"))
    return(NA_integer_)
  }

  # check file has a pdf extension
  if(fs::path_ext(pdf_path) != "pdf") {
    message(glue::glue("{pdf_path} is not a pdf document"))
    return(NA_integer_)
  }

  # count number of pages in pdf document
  pdftools::pdf_info(pdf_path)$pages
}

#' @title
#' Remove pdf document pages
#'
#' @description
#' This function removes one or more pages from a pdf document, saving the original
#' updated document (adding original_updated to the document name) and saving
#' the removed pages as a separate pdf document (adding removed_pages to the document name).
#' The path to the pdf document is requested if not provided.
#'
#' @family pdf
#'
#' @param pdf_path path character string of pdf document.
#' @param pages_to_remove numeric vector of page numbers to extract.
#'
#' @return path character string of pdf document.
#' @export
remove_pdf_document_pages <- function(pdf_path = NULL, pages_to_remove) {

  # select pdf document if not provided
  if(is.null(pdf_path)) {
    pdf_path <- choose.files(caption = "Please select pdf document",
                             multi = FALSE, filters = Filters[c("pdf"), ])
  }

  # check file exists
  if(!fs::file_exists(pdf_path)) {
    message(glue::glue("{pdf_path} does not exist"))
    return(NA_integer_)
  }

  # check file has a pdf extension
  if(fs::path_ext(pdf_path) != "pdf") {
    message(glue::glue("{pdf_path} is not a pdf document"))
    return(NA_integer_)
  }

  # get path of updated pdf document outputs
  pdf_path_output <- fs::path(fs::path_dir(pdf_path),
                              glue::glue("{fs::path_ext_remove(fs::path_file(pdf_path ))}"))

  # remove pdf pages from document and save original document
  pdftools::pdf_subset(input = pdf_path,
                       pages = dplyr::setdiff(1:count_pdf_document_pages(pdf_path), pages_to_remove),
                       output = glue::glue("{pdf_path_output}_original_updated.pdf"))

  # extract pdf pages from original document
  pdftools::pdf_subset(input = pdf_path,
                       pages = dplyr::intersect(1:count_pdf_document_pages(pdf_path), pages_to_remove),
                       output = glue::glue("{pdf_path_output}_removed_pages.pdf"))

  # return pdf path invisibly
  invisible(pdf_path)
}

