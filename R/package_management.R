#' @title
#' Get CRAN packages' system dependencies
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function extracts the list of CRAN packages within a renv lock file, querying
#' the [RStudio Package Manager API](https://packagemanager.rstudio.com/__api__/swagger/index.html),
#' from the [Rstudio Public Package Manager](https://packagemanager.rstudio.com)
#' to get a list of external system libraries these packages depend on.
#'
#' The returned list of system dependencies are intended to help create a Docker file
#' used to build a Docker image for a R project.
#'
#' @seealso
#' The function is a collation and modification of the functions in the [getsysres package](https://github.com/mdneuzerling/getsysreqs/tree/main/R)
#' written by David Neuzerling.
#'
#' @param renv_lock_path Path to the renv.lock file.
#' @param distribution Operating system to be used by the Docker image, with ubuntu as
#' the default. Other operating systems include centos.
#' @param release Release version of the operating system. This may default to the latest
#' operating system release.
#'
#' @return An httr response, containing the packages requiring system dependencies, the
#' system dependencies required and their install command, within a list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_cran_package_system_dependencies(renv_lock_path = "renv.lock",
#'                                      distribution = "unbuntu",
#'                                      release = "20.04")
#' }
get_cran_package_system_dependencies <- function(renv_lock_path,
                                                 distribution = "ubuntu",
                                                 release = NULL) {

  # check renv.lock path exists
  if (!fs::file_exists(renv_lock_path)) {
    stop("Could not find renv lockfile: ", renv_lock_path)
  }

  # extract CRAN packages list as vector
  cran_packages <- purrr::map(jsonlite::fromJSON(renv_lock_path)$Packages, function(x){
    if (x$Source == "Repository" && x$Repository == "CRAN")
      x$Package
    else
      NULL
  }) %>%
    purrr::flatten_chr()

  # create RStudio Package Manager API request
  request <- stringr::str_glue("http://packagemanager.rstudio.com/__api__/repos/1/sysreqs?all=false&pkgname={glue::glue_collapse(cran_packages, sep = '&pkgname=')}&distribution={distribution}")

  if (!is.null(release)) {
    request <- stringr::str_glue("{request}&release={release}")
  }

  # send request
  response <- httr::GET(request)

  # check request has been successful
  status_code <- httr::status_code(response)

  # return error message if request is not successful
  if (status_code != 200) {
    error_message <- stringr::str_glue("Status code {status_code}")
    if (httr::has_content(response)) {
      error_message <- stringr::str_glue("{error_message} : {httr::content(response, 'text')}")
    }
    stop(error_message)
  }

  # return package's system dependencies if request has been successful
  jsonlite::fromJSON(httr::content(response, "text"))
}
