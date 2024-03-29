#' @title
#' Tidy spatial data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function tidies spatial data converting the attribute columns to snake case,
#' set the supplied coordinate reference system, extracting the polygons from a
#' geometry collection and optionally checking for invalid geometry.
#'
#' @param sf_data The spatial data frame to tidy as a sf spatial object.
#' @param epsg_number EPSG number as an integer.
#' @param check_valid Should the invalid geometry by checked (TRUE) or not as the default (FALSE).
#'
#' @return The tidied spatial data frame returned as a sf spatial object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tidy_spatial_data(sf_data = uk_ireland_base_map, epsg_number = 27700, check_valid = TRUE)
#' }
tidy_spatial_data <- function(sf_data, epsg_number, check_valid = FALSE) {

  # Make column names snake_case
  sf_data <- sf_data %>%
    janitor::clean_names()

  # Set CRS
  if (is.na(sf::st_crs(sf_data[["epsg_number"]]))) {
    sf_data <- sf::st_set_crs(sf_data, epsg_number)
  } else if (sf::st_crs(sf_data[["epsg_number"]] != epsg_number)) {
    sf_data <- sf::st_transform(sf_data, espg_number)
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
      stringr::str_glue("Invalid feature: {unlist(feature_invalid)}")
    }
  }

  return(sf_data)
}

#' @title
#' Extract and dissolve polygons from geometry collection
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function extracts polygons from a geometry collection, dissolving them into
#' multipolygons.
#'
#' @param sf_data The spatial data frame as a sf spatial object, containing geometry collections.
#'
#' @return The spatial data frame as a sf spatial object, containing extracted multipolygons.
#'
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

#' @title
#' Create bounding box polygon
#'
#' @description
#' This function creates a bounding box polygon from it's corner coordinates,
#' setting the coordinate reference system from the supplied [EPSG value](https://epsg.org/home.html).
#'
#' @seealso
#' The function was created following the approach taken in the stack overflow
#' [polygons from coordinates](https://stackoverflow.com/questions/44335246/polygons-from-coordinates).
#'
#' @param xmin The left corners easting coordinate as an integer or double.
#' @param ymin The lower corners northing coordinate as an integer or double.
#' @param xmax The right corners easting coordinate as an integer or double.
#' @param ymax The upper corners northing coordinate as an integer or double.
#' @param epsg_number EPSG number as an integer.
#'
#' @return The bounding box polygon returned as a sf spatial object.
#'
#' @export
#'
#' @examples
#' create_bounding_box(xmin = -10.81,
#'                     ymin = 49.85,
#'                     xmax = 2.07,
#'                     ymax = 60.96,
#'                     epsg = 4326)
create_bounding_box <- function(xmin, ymin, xmax, ymax, epsg_number) {

  # create corner points
  lower_left_corner <- sf::st_point(c(xmin, ymin))
  upper_left_corner <- sf::st_point(c(xmin, ymax))
  upper_right_corner <- sf::st_point(c(xmax, ymax))
  lower_right_corner <- sf::st_point(c(xmax, ymin))

  # create multipoint geometry from points
  corner_points <- sf::st_multipoint(rbind(lower_left_corner,
                                           upper_left_corner,
                                           upper_right_corner,
                                           lower_right_corner,
                                           lower_left_corner))

  # create square polygon from multipoint geometry
  square_polygon <- sf::st_polygon(list(corner_points))

  # create sf object
  sf::st_sf(id = 1L, geom = sf::st_sfc(square_polygon), crs = epsg_number)

}

#' @title
#' Get multipolygon spatial dimensions
#'
#' @description
#' This functions returns the co-ordinate reference system, area, perimeter length
#' and x and y centroid co-ordinates of a spatial data frame containing multipolygons.
#' It may be used to compare difference versions of a spatial dataset.
#'
#' @param .data spatial data frame containing a feature_code and feature_name columns,
#'  along with a sf geometry column and co-ordinate reference system
#'
#' @return data frame containing original feature_code and feature_name fields, along with
#' calculated EPSG code, area, length and x and y centroid co-ordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' get_multipolygon_spatial_dimensions(sf_data_frame)
#' }
get_multipolygon_spatial_dimensions <- function(.data) {

  # Check all features are multipolygons
  assertthat::assert_that(sf::st_geometry_type(.data, by_geometry = FALSE) == "MULTIPOLYGON",
                          msg = "All features need to be multipolygons")

  # Check feature_code column exists
  assertthat::assert_that(any(names(.data) == 'feature_code'),
                          msg = "feature_code column containing feature ids needs to be present")

  # Check feature_name column exists
  assertthat::assert_that(any(names(.data) == 'feature_name'),
                          msg = "feature_name column containing feature name needs to be present")

  # Check CRS present
  assertthat::assert_that(!is.na(st_crs(.data)$epsg),
                          msg = "Co-ordinate Reference System needs to be present")

  # Get feature spatial dimensions
  .data %>%
    dplyr::mutate(crs = sf::st_crs(.)$epsg) %>%
    dplyr::mutate(area = sf::st_area(.) %>%
                    round()) %>%
    dplyr::mutate(centroid = sf::st_centroid(.)$geometry,
                  x = sf::st_coordinates(centroid)[, 1] %>%
                    round(),
                  y = sf::st_coordinates(centroid)[, 2] %>%
                    round()) %>%
    dplyr::mutate(length = sf::st_cast(., "MULTILINESTRING") %>%
                    sf::st_length() %>%
                    round()) %>%
    units::drop_units() %>%
    sf::st_drop_geometry() %>%
    dplyr::select(feature_code, feature_name, crs, area, length, x, y) %>%
    dplyr::arrange(feature_code)
}

#' @title
#' UK Ireland Base Map derived from the Oil and Gas Authority's OGA and Lloyd's Register
#' SNS Regional Geological Maps (Open Source) layer
#'
#' @description
#' A simple features data frame of the British and Irish coastline derived from
#' the Oil and Gas Authority's OGA and Lloyd's Register SNS Regional Geological Maps
#' (Open Source) layer.
#'
#' @details
#' UKIrelandIoM_DECC_OSGB36 This dataset contains generalised boundaries for the
#' four countries of the UK, the Isle of Man, and Ireland. The dataset is suitable
#' as a reference for simple background mapping, and is re-usable under the Open
#' Government Licence v3.
#'
#' This boundary dataset was created from a subset of the DECC_OFF_Coastline_ED50
#' layer published by the Oil & Gas Authority in the following data package:
#'
#' [OGA and Lloyd's Register SNS Regional Geological Maps Open Data](http://data-ogauthority.opendata.arcgis.com/datasets?q=OGA+and+Lloyds+Register+SNS+Regional+Geological+Maps).
#'
#' The data has been converted from ED50 to OSGB36 and numerous individual polygons
#' have been merged to create a set of polygons for each of Scotland, England, Wales,
#' Northern Ireland, the Isle of Man, and Ireland.
#'
#' @section Licence:
#' [Open Government Licence v3 (OGL)](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
#' Attribution: Contains data © 2017 Oil & Gas Authority
#'
#' @section Figures:
#' \if{html}{\figure{uk_ireland_base_map.png}{options: width=80\%}}
#'
#' @source
#' Created by Owen Boswarva, 2018-07-04. Downloadable from [DataAdaptive](https://www.datadaptive.com/?pg=14)
#'
#' @format A simple feature data frame with 6 rows and 3 variables
#' \describe{
#'   \item{country}{The name of the country.}
#'   \item{uk}{Is the country in the United Kingdom or not.}
#'   \item{geometry}{The country's polygon geometry.}
#' }
#'
#' @examples
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(here)
#'     library(fs)
#'     library(tmap)
#'   })
#' })
#'
#' # create temp directory
#' dir_create(path(tempdir(), "figures"))
#'
#' # save UK Ireland base map as shape file
#' uk_ireland_base_map %>% sf::st_write(path(tempdir(), "uk_ireland_base_map.shp"),
#'                                          delete_layer = TRUE, quiet = TRUE)
#'
#' # create UK Ireland base map image
#' uk_ireland <- tm_shape(uk_ireland_base_map) +
#'   tm_fill(col = "uk", style = "cat", palette = c("lightgreen", "darkgreen")) +
#'   tm_borders(col = "black") +
#'   tm_compass(type = "8star", size = 4,
#'              position = c(0.8, 0.85)) +
#'   tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.8,
#'                position = c(0.68, 0.02)) +
#'   tm_layout(bg.color = "lightblue",
#'             frame.double.line = TRUE,
#'             inner.margin = 0.05,
#'             outer.margin = 0.05,
#'             legend.show = FALSE,
#'             title = "UK and Ireland base map",
#'            title.size = 1.5,
#'             title.fontface = "bold")
#'
#' # save UK Ireland base map image
#' suppressMessages({uk_ireland %>% tmap_save(path(tempdir(), "figures", "uk_ireland_base_map.png"),
#'                          type = "cairo-png",
#'                          width = 8, height = 8,
#'                          units = "in", dpi = 72)})
#'
#' # move figure from temporary directory
#' if(dir_exists(here("man", "figures"))) {
#'   file_move(path(tempdir(), "figures", "uk_ireland_base_map.png"),
#'             here("man", "figures", "uk_ireland_base_map.png"))
#' }
"uk_ireland_base_map"

