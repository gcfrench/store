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
        class(gridref) <- "gridref"
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
      class(gridref) <- "gridref"
    }
    return(gridref)
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

#' Convert OSGB or OSNI Grid reference to polygon geometry feature
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


#' Manipulate OSGB or OSNI grid reference string
#'
#' @description
#' Extracts grid reference strings at various precisions from the supplied grid
#' reference string - if possible! For example, if you supply a 1km square reference
#' TL2998, then you could get the 10km square TL29, but not a 100m square grid reference.
#'
#' function from [archived rnbn package](https://github.com/ropensci-archive/rnbn/issues/37)
#'
#' @seealso
#' Tetrads are 2x2km squares and are often used for mapping distributions at a the
#' scale of a county or similar sized local area. They are labelled using the 10km
#' square followed by a single, upper-case letter (since there are 25 tetrads in a
#' 10km square, the letter "O" is not used to avoid confusion with zero). This is
#' named the DINTY system after the letters in the second row of this table.
#'
#' 5x5km squares (sometimes called "pentads") are used for mapping at a regional scale.
#' They are labelled using the name of the 10km square followed by two upper-case letters.
#'
#' @author Stuart Ball
#'
#' @param grid character, the grid reference to be manipulated
#' @param format character, the format you want back. The possibilities are: sq10km,
#' sq5km, tetrad, sq1km, sq100m, sq10m
#'
#' @return a list of class "gridref" with the following contents:
#' * the original grid reference
#' * the grid reference string formatted as requested
#' * the grid reference system, either "OSGB" or "OSNI"
#' * the precision of the formatted grid reference in metres
#'
#' @examples
gridRef <- function(format){

  function(grid) {

    tetradLetter <- function(nums, n) {
      x2 <- as.integer(substr(nums, 2, 2))
      y2 <- as.integer(substr(nums, n + 2, n + 2))
      l <- (x2%/%2) * 5 + (y2%/%2)
      return(substr("ABCDEFGHIJKLMNPQRSTUVWXYZ", l +
                      1, l + 1))
    }
    pentadLetter <- function(nums, n) {
      x2 <- as.integer(substr(nums, 2, 2))
      y2 <- as.integer(substr(nums, n + 2, n + 2))
      l <- (x2%/%5) * 2 + (y2%/%5)
      return(c("SW", "NW", "SE", "NE")[l +
                                         1])
    }
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
        gret <- list()
        class(gret) <- "gridref"
        gret$grid <- grid
        ifelse(nchar(letters) == 2, gret$system <- "OSGB",
               gret$system <- "OSNI")
        switch(format, sq10km = {
          if (n > 0) {
            gret$gfmt <- paste(letters, substr(nums, 1,
                                               1), substr(nums, n + 1, n + 1), sep = "")
            gret$precision <- 10000
          } else {
            gret$gfmt <- NA_character_
            gret$precision <- NA_integer_
          }
        }, sq5km = {
          if (n > 1) {
            gret$gfmt <- paste(letters, substr(nums, 1,
                                               1), substr(nums, n + 1, n + 1), pentadLetter(nums,
                                                                                            n), sep = "")
            gret$precision <- 5000
          } else {
            gret$gfmt <- NA_character_
            gret$precision <- NA_integer_
          }
        }, tetrad = {
          if (n > 1) {
            gret$gfmt <- paste(letters, substr(nums, 1,
                                               1), substr(nums, n + 1, n + 1), tetradLetter(nums,
                                                                                            n), sep = "")
            gret$precision <- 2000
          } else {
            gret$gfmt <- NA_character_
            gret$precision <- NA_integer_
          }
        }, sq1km = {
          if (n > 1) {
            gret$gfmt <- paste(letters, substr(nums, 1,
                                               2), substr(nums, n + 1, n + 2), sep = "")
            gret$precision <- 1000
          } else {
            gret$gfmt <- NA_character_
            gret$precision <- NA_integer_
          }
        }, sq100m = {
          if (n > 2) {
            gret$gfmt <- paste(letters, substr(nums, 1,
                                               3), substr(nums, n + 1, n + 3), sep = "")
            gret$precision <- 100
          } else {
            gret$gfmt <- NA_character_
            gret$precision <- NA_integer_
          }
        }, sq10m = if (n > 3) {
          gret$gfmt <- paste(letters, substr(nums, 1, 4),
                             substr(nums, n + 1, n + 4), sep = "")
          gret$precision <- 10
        } else {
          gret$gfmt <- NA_character_
          gret$precision <- NA_integer_
        }, message("requested output format not recognised"))
      }
      else {
        message("must be an even number of digits")
        gret <- list(grid = NA_character_,
                     system = NA_character_,
                     gfmt = NA_character_,
                     precision = NA_integer_)
        class(gret) <- "gridref"
      }
    }
    else {
      message("not a valid grid reference string")
      gret <- list(grid = NA_character_,
                   system = NA_character_,
                   gfmt = NA_character_,
                   precision = NA_integer_)
      class(gret) <- "gridref"
    }

    return(gret[["gfmt"]])
  }
}

#' Get 10km grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the 10km grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return character, 10km grid reference
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add ten_km column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(ten_km = grid_reference_hectad(grid_reference))
#'}
grid_reference_hectad <- gridRef(format = "sq10km")

#' Get 5km grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the 5km grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return character, 5km grid reference
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add five_km column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(five_km = grid_reference_pentad(grid_reference))
#'}
grid_reference_pentad <- gridRef(format = "sq5km")

#' Get 2km grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the 2km grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return character, 2km grid reference
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add two_km column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(five_km = grid_reference_tetrad(grid_reference))
#'}
grid_reference_tetrad <- gridRef(format = "tetrad")

#' Get 1km grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the 1km grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return character, 1km grid reference
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add one_km column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(one_km = grid_reference_monad(grid_reference))
#'}
grid_reference_monad <- gridRef(format = "sq1km")

#' Get 100m grid reference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the 100m grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character, British or Irish grid reference
#'
#' @return character, 100m grid reference
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # add one_hundred_m column
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(one_hundred_m = grid_reference_hectare(grid_reference))
#'}
grid_reference_hectare <- gridRef(format = "sq100m")



