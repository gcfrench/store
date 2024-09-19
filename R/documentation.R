# quarto -----------------------------------------------------------------------

#' @noRd
build_quarto_article <- function(all_files) {

  function(...) {

    # function converting quarto to html
    quarto_to_html <- function (quarto_path) {

      # split path into component parts
      directory_name <- quarto_path |>
        fs::path_dir()
      file_name <- quarto_path |>
        fs::path_ext_remove() |>
        fs::path_file()

      # render quarto document as html
      quarto::quarto_render(fs::path(directory_name, glue::glue("{file_name}.qmd")), output_format = "html")

      # move html document to docs/articles or docs/archive/articles folder
      if (stringr::str_detect(directory_name, "archive")) {
        fs::file_move(fs::path(directory_name, glue::glue("{file_name}.html")),
                      fs::path(here::here(), "docs", "articles", "archive", glue::glue("{file_name}.html")))
      } else {
        fs::file_move(fs::path(directory_name, glue::glue("{file_name}.html")),
                      fs::path(here::here(), "docs", "articles", glue::glue("{file_name}.html")))
      }
    }

    if (all_files) {

      # iterate through all quarto documents
      fs::dir_ls("quarto_docs", glob = "*.qmd", recurse = TRUE) |>
        purrr::walk(quarto_to_html)
    } else {

      # choose single quarto document
      fs::path(choose.files(default = "quarto_docs",
                            caption = "Select quarto document", multi = FALSE)) |>
        quarto_to_html()
    }
  }
}

#' @title
#' Build a single quarto document into a html document
#'
#' @description
#' This function builds a selected quarto document into a html document, moving
#' the html file into the articles sub-folder of the doc folder so that it can be
#' included in package documentation, created by the pkgdown package.
#'
#' @export
build_single_quarto_article <- build_quarto_article(all_files = FALSE)


#' @title
#' Build all quarto documents in the vignettes folder into html documents
#'
#' @description
#' This function builds all the quarto documents in the package's vignettes folder
#' into a html documents, moving these html documents into the articles sub-folder
#' of the doc folder so that they can be included in package documentation,
#' created by the pkgdown package.
#'
#' @details
#' use this function after running pkgdown's build_site function to create the
#' article html documents
#'
#' @export
build_all_quarto_articles <- build_quarto_article(all_files = TRUE)

# markdown ---------------------------------------------------------------------

#' @noRd
build_markdown_article <- function(all_files) {

  function(...) {

    # function converting markdown to html
    markdown_to_html <- function(markdown_path) {

      # split path into component parts
      directory_name <- markdown_path |>
        fs::path_dir()
      file_name <- markdown_path |>
        fs::path_ext_remove() |>
        fs::path_file()

      message(file_name)

      # render markdown document as html
      pkgdown::build_article(file_name)

    }

    if(all_files) {

      # iterate through all markdown documents
      pkgdown::build_articles()
    } else {

      # choose single markdown document
      fs::path(choose.files(default = "vignettes",
                            caption = "Select markdown document", multi = FALSE)) |>
        markdown_to_html()
    }
  }
}

#' @title
#' Build a single markdown document into a html document
#'
#' @description
#' This function builds a selected markdown document into a html document, moving
#' the html file into the articles sub-folder of the doc folder so that it can be
#' included in package documentation, created by the pkgdown package.
#'
#' @export
build_single_markdown_article <- build_markdown_article(all_files = FALSE)

#' @title
#' Build all markdown documents in the vignettes folder into html documents
#'
#' @description
#' This function builds all the markdown documents in the package's vignettes folder
#' into a html documents, moving these html documents into the articles sub-folder
#' of the doc folder so that they can be included in package documentation,
#' created by the pkgdown package.
#'
#' @export
build_all_markdown_articles <- build_markdown_article(all_files = TRUE)

#' @title
#' Create data project documentation
#'
#' @description
#' This function creates the documentation pages for the data project
#'
#' @details
#' Currently home page creation results in an unable to read RDS error and has been
#' commented out
#'
#' @export
build_documentation <- function() {

  # update package
  devtools::load_all()

  # remove previous site
  pkgdown::clean_site()

  # create Rmarkdown vignettes
  pkgdown::build_articles()

  # create quarto documents
  build_all_quarto_articles()

  # create function and lookup reference documentation
  pkgdown::build_reference()

  # create news
  pkgdown::build_news()

  # create home page
  pkgdown::build_home()
}

#' @title
#' Build child project
#'
#' @description
#' This function updates the package ready for backing up as a child project, including
#' creating an up-to-date renv lock file, checking for and adding package dependencies to
#' the description file and updating the package version
#'
#' @export
build_child_project <- function() {

  # update renv lock file
  renv::snapshot(prompt = FALSE)

  # check and add missing package dependencies to description file
  attachment::att_amend_desc()

  # update version
  usethis::use_version(which = "patch")

}



