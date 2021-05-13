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

#' Create bounding box polygon
#'
#' @description
#' This function creates a bounding box polygon from it's corner coordinates,
#' setting the coordinate reference system from the supplied [EPSG value](https://epsg.org/home.html)
#'
#' @seealso
#' see stack overflow [polygons from coordinates](https://stackoverflow.com/questions/44335246/polygons-from-coordinates)
#'
#' @param xmin integer, left corner coordinate
#' @param ymin integer, lower corner coordinate
#' @param xmax integer, right corner coordinate
#' @param ymax integer, upper corner coordinate
#'
#' @return sf object, bounding box polygon
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'    library(here)
#'    library(fs)
#'   })
#'})
#'# create output directory
#'i_am("example.Rmd")
#'if (!dir_exists("output")) {dir_create("output")}
#'
#'create_bounding_box(xmin = -10.81,
#'                    ymin = 49.85,
#'                    xmax = 2.07,
#'                    ymax = 60.96,
#'                    epsg = 4326) %>%
#' sf::st_write("output/uk_ireland_bounding_box.shp")
#' }
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

#' UK Ireland Base Map derived from the Oil and Gas Authority's OGA and Lloyd's Register
#' SNS Regional Geological Maps (Open Source) layer
#'
#' @description
#' A simple features data frame of the British and Irish coastline
#' derived from the Oil and Gas Authority's OGA and Lloyd's Register
#' SNS Regional Geological Maps (Open Source) layer.
#'
#' UKIrelandIoM_DECC_OSGB36 This dataset contains generalised boundaries for the
#' four countries of the UK, the Isle of Man, and Ireland. The dataset is suitable
#' as a reference for simple background mapping, and is re-usable under the Open
#' Government Licence v3.
#'
#' This boundary dataset was created from a subset of the DECC_OFF_Coastline_ED50
#' layer published by the Oil & Gas Authority in the following data package:
#'
#' OGA and Lloyd's Register SNS Regional Geological Maps (Open Source)
#' http://data-ogauthority.opendata.arcgis.com/datasets?q=OGA+and+Lloyd%27s+Register+SNS+Regional+Geological+Maps&sort_by=relevanc
#'
#' The data has been converted from ED50 to OSGB36 and numerous individual polygons
#' have been merged to create a set of polygons for each of Scotland, England, Wales,
#' Northern Ireland, the Isle of Man, and Ireland.
#'
#' Licence: Open Government Licence v3 (OGL)
#' http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#' Attribution: Contains data © 2017 Oil & Gas Authority
#'
#' Created by Owen Boswarva, 2018-07-04
#' Downloadable from [DataAdaptive](https://www.datadaptive.com/?pg=14)
#'
#' @examples
#' \dontrun{
#'suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'    library(here)
#'    library(fs)
#'    library(tmap)
#'  })
#'})
#'# create output directory
#'i_am("example.Rmd")
#'if (!dir_exists("output")) {dir_create("output")}
#'
#'# save UK Ireland base map as shape file
#'uk_ireland_base_map %>% sf::st_write("output/uk_ireland_base_map.shp")
#'
#'# create UK Ireland base map image
#'uk_ireland <- tm_shape(uk_ireland_base_map) +
#'  tm_fill(col = "uk", style = "cat", palette = c("lightgreen", "darkgreen")) +
#'  tm_borders(col = "black") +
#'  tm_compass(type = "8star", size = 4,
#'             position = c(0.8, 0.85)) +
#'  tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.8,
#'               position = c(0.68, 0.02)) +
#'  tm_layout(bg.color = "lightblue",
#'            frame.double.line = TRUE,
#'            inner.margin = 0.05,
#'            outer.margin = 0.05,
#'            legend.show = FALSE,
#'           title = "UK and Ireland base map",
#'            title.size = 1.5,
#'            title.fontface = "bold")
#'
#'# save UK Ireland base map image
#'uk_ireland %>%  tmap_save("output/uk_ireland_base_map.png", type = "cairo-png",
#'                         width = 8, height = 8, units = "in", dpi = 72)
#' }
#' @format A simple feature data frame with 6 rows and 3 variables
#' \describe{
#'   \item{country}{country name}
#'   \item{uk}{whether country is in UK or not}
#'   \item{geometry}{polygon genometry}
#' }
"uk_ireland_base_map"

