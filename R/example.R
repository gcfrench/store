#' @title
#' NBN demonstration dataset
#'
#' @description
#' This dataset contains sightings of 10 common birds species within three national
#' nature reserves, Monks Wood, Woodwalton Fen and Holme Fen, occuring in or adjacent
#' to TL28 10km grid square, recorded in 2008 for the purpose of demonstrating the
#' functionality of the NBN Gateway. This website collated and made freely available
#' biological species records across the UK. It was superseded in March 2017 by
#' the [NBN Atlas](https://nbnatlas.org/).
#'
#' @section Figures:
#' Sample of 10 rows from nbn_demonstration_dataset dataset.
#'
#' \if{html}{\figure{nbn_demonstration_dataset.png}{options: width=80\%}}
#'
#' @format
#' A tibble with `r nrow(nbn_demonstration_dataset)` rows and `r ncol(nbn_demonstration_dataset)` variables.
#' \describe{
#'   \item{RecordKey}{Primary key associated with the occurrence record.}
#'   \item{SurveyKey}{Identifier key for the survey.}
#'   \item{StartDate}{Start date of vague date range.}
#'   \item{EndDate}{End date of vague date range.}
#'   \item{DateType}{Date type of vague date range.}
#'   \item{Sensitive}{Whether the record is sensitive or not.}
#'   \item{TaxonVersionKey}{Identifier key for taxon version in UK Species Inventory.}
#'   \item{GridReference}{Grid reference of occurrence record.}
#'   \item{Precision}{Spatial precision of the georeference in metres.}
#'   \item{Projection}{Projection system for the grid reference.}
#'   \item{SiteKey}{Identifier key for the site.}
#'   \item{SiteName}{Name of the site or location where the species was recorded.}
#'   \item{Recorder}{Name or list of names for one or more recorders for the species record.}
#'   \item{Determiner}{Name or list of names for one or more determiners for the species record.}
#'   \item{Abundance}{Attribute field containing number of individuals recorded.}
#'   \item{TaxonName}{Attribute field containing scientific name of species recorded.}
#'   \item{CommonName}{Attribute field containing common name of species recorded.}
#' }
#'
#' @examples
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'   library(dplyr)
#'   library(gt)
#'   library(here)
#'   library(fs)
#'   library(pagedown)
#'  })
#' })
#'
#' # create figures directory
#' dir_create(path(tempdir(), "figures"))
#'
#' # create html table
#' nbn_demonstration_dataset %>%
#'   slice_sample(n = 10) %>%
#'   gt_preview(top_n = 10, incl_rownums = FALSE) %>%
#'   gtsave(path(tempdir(), "figures", "nbn_demonstration_dataset.html"))
#'
#' # convert to image
#' chrome_print(path(tempdir(), "figures", "nbn_demonstration_dataset.html"),
#'              format = "png")
#'
#' # move image
#' file_move(path(tempdir(), "figures", "nbn_demonstration_dataset.png"),
#'          here("man", "figures", "nbn_demonstration_dataset.png"))
"nbn_demonstration_dataset"
