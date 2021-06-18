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
#' @format
#' A tibble with `r nrow(nbn_demonstration_dataset)` rows and `r ncol(nbn_demonstration_dataset)` variables
#' \describe{
#'   \item{RecordKey}{primary key associated with the occurrence record}
#'   \item{SurveyKey}{identifier key for the survey}
#'   \item{StartDate}{start date of vague date range}
#'   \item{EndDate}{end date of vague date range}
#'   \item{DateType}{date type of vague date range}
#'   \item{Sensitive}{whether the record is sensitive or not}
#'   \item{TaxonVersionKey}{identifier key for taxon version in UK Species Inventory}
#'   \item{GridReference}{grid reference of occurrence record}
#'   \item{Precision}{spatial precision of the georeference in metres}
#'   \item{Projection}{projection system for the grid reference}
#'   \item{SiteKey}{identifier key for the site}
#'   \item{SiteName}{name of the site or location where the species was recorded}
#'   \item{Recorder}{name or list of names for one or more recorders for the species record}
#'   \item{Determiner}{name or list of names for one or more determiners for the species record}
#'   \item{Abundance}{attribute field containing number of individuals recorded}
#'   \item{TaxonName}{attribute field containing scientific name of species recorded}
#'   \item{CommonName}{attribute field containing common name of species recorded}
#' }
"nbn_demonstration_dataset"
