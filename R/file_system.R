#' @title
#' Copy list of file names to clipboard
#'
#' @description
#' This function copies the list of file names contained within a directory and
#' it's child directories to the clipboard.
#'
#' @return console message reporting list of files names copied to the clipboard
#' @export
filenames_to_clipboard <- function() {

  fs::dir_ls(choose.dir(caption = "Select parent folder containing files"),
             recurse = TRUE, type = "file") %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    clipr::write_clip()

  message("List of the file names copied to the clipboard")
}


