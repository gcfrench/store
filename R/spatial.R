#' Tidy spatial data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function tidies spatial data converting the attribute columns to snake case,
#' set the supplied coordinate reference system, extracting the polygons from a
#' geometry collection and optionally checking for invalid geometry
#'
#' @param sf_data sf object, spatial data frame to tidy
#' @param epsg integer, EPSG number
#' @param check_valid logical, whether to check for invalid geometry (TRUE) or not (default)
#'
#' @return sf object, tidied spatial data frame
#' @export
#'
#' @examples
#' \dontrun{
#' tidy_spatial_data(sf_data = uk_ireland_base_map, epsg = 27700, check_valid = TRUE)
#' }
tidy_spatial_data <- function(sf_data, epsg, check_valid = FALSE) {

  # Make column names snake_case
  sf_data <- sf_data %>%
    janitor::clean_names()

  # Set CRS
  if (is.na(sf::st_crs(sf_data[["epsg"]]))) {
    sf_data <- sf::st_set_crs(sf_data, epsg)
  } else if (sf::st_crs(sf_data[["epsg"]] != epsg)) {
    sf_data <- sf::st_transform(sf_data, espg)
  }

  # Extract multipolygons from GeometryCollection
  if (any(sf::st_geometry_type(sf_data) == "GEOMETRYCOLLECTION" )) {

    # Extract polygons and then dissolve by feature_code into multipolygons
    # For explanation of dissolving by attribute see https:/github.com/r-spatial/sf/issues/290
    feature_gc <- sf_data %>%
      dplyr::filter(sf::st_geometry_type(.) == "GEOMETRYCOLLECTION") %>%
      sf::st_collection_extract("POLYGON") %>%
      dplyr::group_by(feature_code) %>%
      dplyr::summarize()

    # Re-append feature_codes with multipolygons
    sf_data <- sf_data %>%
      dplyr::filter(sf::st_geometry_type(.) != "GEOMETRYCOLLECTION") %>%
      dplyr::bind_rows(feature_gc)
  }

  # Check for invalid Geometry
  if (check_valid) {
    feature_invalid <- sf_data[!sf::st_is_valid(sf_data), ]
    sf::st_geometry(feature_invalid) = NULL
    if (assertthat::not_empty(feature_invalid)) {
      stingr::str_glue("Invalid feature: {unlist(feature_invalid)}")
    }
  }

  return(sf_data)
}

#' Extract and dissolve polygons from geometry collection
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function extracts polygons from a geometry collection, dissolving them into
#' multipolygons
#'
#' @param sf_data sf object, spatial data frame containing geometry collections
#'
#' @return sf object, spatial data frame containing extracted multipolygons
#' @export
#'
#' @examples
#' \dontrun{
#' extract_polygons(uk_ireland_base_map)
#' }
extract_polygons <- function(sf_data) {

  # Extract polygons and then dissolve by feature_code into multipolygons
  # For explanation of dissolving by attribute see https:/github.com/r-spatial/sf/issues/290
  if (any(sf::st_geometry_type(sf_data) == "GEOMETRYCOLLECTION" )) {

    # Add running integer
    sf_data <- sf_data %>%
     dplyr::mutate(feature_id = row_number()) %>%
     dplyr::relocate(feature_id, .before = dplyr::everything())

    # Extract polygons
    feature_gc <- sf_data %>%
      dplyr::filter(sf::st_geometry_type(.) == "GEOMETRYCOLLECTION") %>%
      sf::st_collection_extract("POLYGON") %>%
      dplyr::group_by(feature_id) %>%
      dplyr::summarize()

    # Re-append feature_codes with multi-polygons
    sf_data <- sf_data %>%
      dplyr::filter(sf::st_geometry_type(.) != "GEOMETRYCOLLECTION") %>%
      dplyr::bind_rows(feature_gc)
  }

  return(sf_data)
}
