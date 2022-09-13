#' @title
#' Merge pdf documents
#'
#' @description
#' This function merges one or more pdf documents, saving the resulting merged
#' pdf document either to a provided path or default merged pdf name.
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
