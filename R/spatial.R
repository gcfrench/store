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
#' including tetrads (2000m precision)
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return geometry, square polygon feature
#' @export
#'
#' @examples
#' \dontrun{
#' grid_reference_to_geometry("SP123456")
#' }
grid_reference_to_geometry <- function(grid_reference) {

  # https://github.com/ropensci-archive/rnbn/issues/37
  gridCoords <- function (grid = NULL, units = c("km", "m"))
  {
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
        return(gridref)
      }
      else {
        stop("must be an even number of digits")
      }
    }
    else {
      stop("not a valid grid reference string")
    }
  }

  # Get easting and northing coordinates using rNBN
  coords <- gridCoords(grid = grid_reference, unit = "m")
  easting <- as.numeric(coords[3])
  northing <- as.numeric(coords[4])
  precision <- as.numeric(coords[5])
  projection <- as.character(coords[2])

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
