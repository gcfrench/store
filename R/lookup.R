#' @title
#' [ISO-3166 three letter country codes with United Nations regions and European Union membership](tables/country_codes_table.html)
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
#' @format A tibble with `r nrow(country_codes)` rows and `r ncol(country_codes)` variables
#' \describe{
#'   \item{alpha_3}{ISO_3166_1 country code}
#'   \item{country_name}{Name of country}
#'   \item{un_region_name}{Name of United Nations region}
#'   \item{eu_member}{Whether the country is a member of the European Union or not}
#' }
#'
#' @examples
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'   library(gt)
#'   library(here)
#'   library(fs)
#'  })
#' })
#'
#' # create output directory
#' i_am("docs/reference/country_codes.html")
#' if (!dir_exists("tables")) {dir_create("tables")}
#'
#' # create html table
#' country_codes %>%
#'   gt(groupname_col = "un_region_name") %>%
#'   tab_header(title = md("**ISO-3166 three letter country codes**"),
#'              subtitle = md("*Includes United Nations regions and European
#'                                 Union membership*")) %>%
#'   cols_hide(eu_member) %>%
#'   tab_style(
#'     style = list(
#'       cell_fill(color = "#000099"), # Pantone Reflex Blue
#'       cell_text(color = "#FFCC00")), # Pantone Yellow
#'     locations = cells_body(rows = eu_member == TRUE)
#'   ) %>%
#'  tab_options(
#'     heading.align = "left",
#'     heading.background.color = "#2C3E50",
#'     column_labels.hidden = TRUE,
#'     row_group.font.weight = "bold",
#'     row_group.background.color = "#2C3E50",
#'     table.width = pct(50),
#'     table.font.size = "smaller",
#'     data_row.padding = px(0)
#'   ) %>%
#'   opt_table_lines("none") %>%
#'   gtsave("tables/country_codes_table.html")
"country_codes"

#' @title
#' UK and Ireland 10km grid squares
#'
#' @description
#' This datasets lists the 10km grid squares occurring in the UK and Ireland, as
#' well as the Channel Islands and Isle of Man. For each 10km grid square which
#' country(s) it occurs in, whether it is in the UK and is on land or in sea is
#' given, as well as the latitude and longitude for the mid centroid point

#' @format A tibble with `r nrow(uk_ireland_tenkm_grid_squares)` rows and `r ncol(uk_ireland_tenkm_grid_squares)` variables
#' \describe{
#'   \item{ten_km}{ten km grid square}
#'   \item{country}{list of countries containing the 10km grid square}
#'   \item{uk}{whether the 10km grid square is in the UK or not}
#'   \item{mid_longitude}{longitude of 10km grid square centroid, to three decimal points}
#'   \item{mid_latitude}{latitude of 10km grid square centroid, to three decimal points}
#'   \item{geographical}{whether the 10km grid square is on land or in the sea}
#' }
"uk_ireland_tenkm_grid_squares"

#' @title
#' Vice counties grid square intersections
#'
#' @description
#' A list of OSGB 10km, 2km and 1km grid squares with the dominant vice county that
#' overlaps the grid square and list of all vice counties overlapping the grid square
#' at the vice county borders.
#'
#' The list of British vice-counties can be downloaded from the [Biological Records Centre](https://www.brc.ac.uk/article/british-vice-counties)
#' website, with the vice-county boundaries downloaded from [Biological Records Centre's github page](https://github.com/BiologicalRecordsCentre/vice-counties)
#'
#' @format A tibble with `r nrow(vc_grid_square_intersects)` rows and `r ncol(vc_grid_square_intersects)` variables
#' \describe{
#'   \item{grid_square}{OSGB 10km, 2km or 1km grid square}
#'   \item{precsion}{precision of grid square in metres}
#'   \item{vc_dominant}{dominant vice county number occupying the largest grid square area}
#'   \item{vc_count}{number of vice counties overlapping the grid square}
#'   \item{vc_list}{hash separated list of vice counties numbers overlapping the grid square}
#' }
"vc_grid_square_intersects"


