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
#' tidy_spatial_data(sf_data = "uk_ireland_base_map", epsg = 27700, check_valid = TRUE)
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

#' Get x,y coordinates from a grid reference
#'
#' @description
#' Given an OSGB or OSNI grid reference string, get the x,y coordinates of the OSGB
#' or OSNI grid for the bottom, left-hand corner of the grid square. The units
#' parameter controls the units (metres m or kilometres km) in which the coordinates
#' should be returned
#'
#' function from [archived rnbn package](https://github.com/ropensci-archive/rnbn/issues/37)
#'
#' @author Stuart Ball
#'
#' @param grid character, an OSGB or OSNI grid reference string
#' @param units character, metres or kilometres (default)
#'
#' @return a list of class "gridref" with the following contents:
#' * grid	the original grid reference
#' * system	the grid reference system, either "OSGB" or "OSNI"
#' * x the x coordinate (easting) in requested units
#' * y the y coordinate (northing)in requested units
#' * units "m" or "km"
#' * precision the precision of the original grid reference in metres
gridCoords <-  function (grid = NULL, units = c("km", "m")) {
    decodeTetrad <- function(letter) {
      l = as.integer(charToRaw(letter)) - 65
      if (l > 13)
        l <- l - 1
      coord <- list()
      coord$x <- (l%/%5) * 2000
      coord$y <- (l%%5) * 2000
      return(coord)
    }
    units <- match.arg(units)
    gr <- toupper(gsub(" ", "", grid))
    v <- regexec("^([H,N,O,S,T][A-H,J-Z]|[B-D,F-J,L-O,Q-T,V-X])([0-9]{2,10})([A-N,P-Z]{0,1})$",
                 gr)
    if (v[[1]][[1]] > 0) {
      letters <- unlist(regmatches(gr, v))[2]
      nums <- unlist(regmatches(gr, v))[3]
      tetrad <- unlist(regmatches(gr, v))[4]
      n <- nchar(nums)
      if ((n%%2) == 0) {
        n <- n%/%2
        precision <- 10^(5 - n)
        gridref <- list()
        class(gridref) <- "gridref"
        gridref$grid <- grid
        if (nchar(letters) == 2) {
          gridref$system <- "OSGB"
        }
        else {
          gridref$system <- "OSNI"
        }
        x <- 0
        y <- 0
        l <- 1
        if (gridref$system == "OSGB") {
          l = as.integer(charToRaw(substr(letters, l, l))) -
            65
          if (l > 7)
            l <- l - 1
          x <- ((l%%5) - 2) * 5e+05
          y <- (3 - (l%/%5)) * 5e+05
          l <- 2
        }
        l = as.integer(charToRaw(substr(letters, l, l))) -
          65
        if (l > 7)
          l <- l - 1
        x <- x + (l%%5) * 1e+05
        y <- y + (4 - (l%/%5)) * 1e+05
        x <- x + as.integer(substr(nums, 1, n)) * precision
        y <- y + as.integer(substr(nums, n + 1, n + 1 + n)) *
          precision
        if (nchar(tetrad) == 1) {
          c <- decodeTetrad(tetrad)
          x <- x + c$x
          y <- y + c$y
          precision <- 2000
        }
        if (units == "km") {
          x <- x/1000
          y <- y/1000
        }
        gridref$x <- x
        gridref$y <- y
        gridref$precision <- precision
        gridref$units <- units
      }
      else {
        message("must be an even number of digits")
        gridref <- list(grid = NA_character_,
                        system = NA_character_,
                        x = NA_integer_,
                        y = NA_integer_,
                        precision = NA_integer_,
                        units = NA_integer_)
      }
    }
    else {
      message("not a valid grid reference string")
      gridref <- list(grid = NA_character_,
                      system = NA_character_,
                      x = NA_integer_,
                      y = NA_integer_,
                      precision = NA_integer_,
                      units = NA_integer_)
    }
    return(gridref)
}

#' Check if grid reference is a valid OSGB or OSNI Grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function checks if the grid reference is a valid OSGB or OSNI grid reference.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return logical, grid reference valid (TRUE) or invalid (FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add valid column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(valid = grid_reference_valid(grid_reference))
#'}
grid_reference_valid <- function(grid_reference) {

  # Get projection using rNBN
  projection <- gridCoords(grid = grid_reference) %>%
    purrr::pluck("system")

  # return valid grid reference if projection returned
  if(!is.na(projection)) {
    TRUE
  } else {
    FALSE
  }
}

#' Get projection for grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the grid reference's projection, either as OSGB or OSNI.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return character, grid reference projection in British National Grid (OSGB) or Irish National Grid (OSNI).
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add projection column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(projection = grid_reference_projection(grid_reference))
#'}
grid_reference_projection <- function(grid_reference) {

  # Get projection using rNBN
  gridCoords(grid = grid_reference) %>%
    purrr::pluck("system")
}

#' Get precison for grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the grid reference's precsion in metres.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return integer, precision of grid reference in metres.
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add precision column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(precision = grid_reference_precision(grid_reference))
#'}
grid_reference_precision <- function(grid_reference) {

  # Get precision using rNBN
  gridCoords(grid = grid_reference, units = "m") %>%
    purrr::pluck("precision")
}

#' Get easting for grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the grid reference's easting in metres.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#' @param centre logical, easting for either lower left hand corner (default) or centre point (TRUE)
#'
#' @return integer, easting of grid reference in metres.
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add easting column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(easting = grid_reference_easting(grid_reference, centre = FALSE))
#'}
grid_reference_easting <- function(grid_reference, centre = FALSE) {

  # Get easting for lower left hand corner using rNBN
  easting <- gridCoords(grid = grid_reference, units = "m") %>%
    purrr::pluck("x")

  # get easting for centre point
  if(centre) {

    # Get precision using rNBN
    precision <- gridCoords(grid = grid_reference, units = "m") %>%
      purrr::pluck("precision")

    easting <- easting + (precision / 2L)
  }

  return(easting)
}

#' Get northing for grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the grid reference's northing in metres.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#' @param centre logical, northing for either lower left hand corner (default) or centre point (TRUE)
#'
#' @return integer, northing of grid reference in metres.
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add northing column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(northing = grid_reference_northing(grid_reference, centre = FALSE))
#'}
grid_reference_northing <- function(grid_reference, centre = FALSE) {

  # Get northing for lower left hand corner using rNBN
  northing <- gridCoords(grid = grid_reference, units = "m") %>%
    purrr::pluck("y")

  # get northing for centre point
  if(centre) {

    # Get precision using rNBN
    precision <- gridCoords(grid = grid_reference, units = "m") %>%
      purrr::pluck("precision")

    northing <- northing + (precision / 2L)
  }

  return(northing)
}

#' Convert OSGB Grid reference to polygon geometry feature
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function converts a grid reference to its square polygon geometry feature
#' through conversion to well-known text. It uses the gridCoords function in the
#' archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision) and returns an empty polygon feature for
#' an invalid grid reference
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return geometry, square polygon feature
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
#' # create output directory
#' i_am("example.Rmd")
#' if (!dir_exists("output")) {dir_create("output")}
#'
#' # create sf data frame and export as shape file
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(geometry = grid_reference_geometry(grid_reference)) %>%
#'   sf::st_write("output/nbn_demonstration_dataset.shp")
#'}
grid_reference_geometry <- function(grid_reference) {

  # Get easting and northing coordinates using rNBN
  coords <- gridCoords(grid = grid_reference, unit = "m")
  easting <- coords %>% purrr::pluck("x")
  northing <- coords %>% purrr::pluck("y")
  precision <- coords %>% purrr::pluck("precision")
  projection <- coords %>% purrr::pluck("system")

  # return empty polygon geometry if invalid grid reference
  if(is.na(projection)) {
    return(sf::st_sfc(sf::st_polygon()))
  }

  # Get EPSG code
  if(projection == "OSGB") {
    epsg = 27700
  } else if (projection == "OSNI") {
    epsg = 29902
  } else {
    epsg = NA_integer_
  }

  # convert coordinates to WKT
  wkt <- stringr::str_glue("POLYGON (({easting} {northing}, {easting + precision} {northing}, {easting + precision} {northing + precision}, {easting} {northing + precision}, {easting} {northing}))") %>%
    vctrs::vec_cast(character())

  # convert to geometry feature
  sf::st_as_sfc(wkt, crs = epsg)
}

