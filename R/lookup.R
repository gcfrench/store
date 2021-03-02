#' ISO-3166 three letter country codes with United Nations regions
#'
#' @description
#' International Organisation for Standardisation three letter codes for the
#' representation of country names taken from the [ISOcodes package](https://cran.r-project.org/web/packages/ISOcodes/index.html)
#' , associated with UN regions taken from [United Nations website](https://unstats.un.org/unsd/methodology/m49/).
#'
#' The missing old European country Yugoslavia was added, along with the UN regions
#' for one Americas country BES Bonaire, Sint Eustatius and Saba, and three Asian
#' countries HKG Hong Kong, MAC Macao and TWN Taiwan, Province of China.
#'
#' @format A tibble with 250 rows and 3 variables
#' \describe{
#'   \item{alpha_3}{ISO_3166_1 country code}
#'   \item{country_name}{Name of country}
#'   \item{un_region_name}{Name of United Nations region}
#' }
"country_codes"
