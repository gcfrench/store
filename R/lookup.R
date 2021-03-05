#' ISO-3166 three letter country codes with United Nations regions and European Union membership
#'
#' @description
#' International Organisation for Standardisation three letter codes for the
#' representation of country names taken from the [ISOcodes package](https://cran.r-project.org/web/packages/ISOcodes/index.html)
#' , associated with UN regions taken from [United Nations website](https://unstats.un.org/unsd/methodology/m49/),
#' and whether the country is the member of the [European Union](https://europa.eu/european-union/about-eu/countries_en)
#'
#' The missing old European country Yugoslavia was added, along with the UN regions
#' for one Americas country BES Bonaire, Sint Eustatius and Saba, and three Asian
#' countries HKG Hong Kong, MAC Macao and TWN Taiwan, Province of China.
#'
#' @format A tibble with 250 rows and 4 variables
#' \describe{
#'   \item{alpha_3}{ISO_3166_1 country code}
#'   \item{country_name}{Name of country}
#'   \item{un_region_name}{Name of United Nations region}
#'   \item{eu_member}{Whether the country is a member of the European Union or not}
#' }
"country_codes"
