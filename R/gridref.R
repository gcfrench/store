# Requires
## grid_references.R example
## test-gridref.R tests
## gridref_api.R

# S3 construction function -----------------------------------------------------

#' @title
#' Convert a string to a gridref object
#'
#' @description
#' Converts a valid grid reference character string to a gridref object.
#'
#' @details
#' Checks that the character string is a valid grid reference before converting to
#' a gridref class. Returns an error if the grid reference is invalid.
#'
#' @seealso
#' Constructor function for gridref class as described in [S3](https://adv-r.hadley.nz/s3.html)
#' chapter of Advanced R second additon by Hadley Wickham.
#'
#' @param grid_reference A Great British or Irish grid reference character string.
#'
#' @return A grid reference character string with an added gridref class.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # check grid_reference class
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(purrr)
#'   })
#' })
#'
#' grid_references %>%
#'  map_chr(., class)
as_gridref <- function(grid_reference = character()) {

  # gridref must be a character string
  stopifnot(is.character(grid_reference))

  # gridref must be a valid grid reference
  validate_gridref(grid_reference)

  # convert to gridref class
  class(grid_reference) <- class(grid_reference) %>%
    append("gridref", after = 0)

  # return grid_reference
  grid_reference
}

# S3 Validator function --------------------------------------------------------

#' @title
#' Validate a grid reference
#'
#' @description
#' Internal validator function to check that a grid reference is valid before
#' converting to a gridref class. Returns an error if the grid reference is invalid.
#'
#' @details
#' Taken from [archived rnbn package](https://github.com/ropensci-archive/rnbn/issues/37).
#'
#' @author Stuart Ball.
#'
#' @inheritParams as_gridref
#'
#' @return A validated grid reference character string.
#' @keywords internal
validate_gridref <- function(grid_reference) {

  gr <- toupper(gsub(" ", "", grid_reference))
  v <- regexec("^([H,N,O,S,T][A-H,J-Z]|[B-D,F-J,L-O,Q-T,V-X])([0-9]{2,10})([A-N,P-Z]{0,1})$",
               gr)
  if (v[[1]][[1]] > 0) {
    if ((nchar(unlist(regmatches(gr, v))[3]) %% 2) != 0) {
      stop("must be an even number of digits")
    }
  }
  else {
    stop("not a valid grid reference string")
  }

  return(grid_reference)
}

# S3 generic functions ---------------------------------------------------------

#' @title
#' Get the x, y coordinates from a grid reference
#'
#' @description
#' Given an OSGB or OSNI grid reference string, get the x, y coordinates of the OSGB
#' or OSNI grid for the bottom, left-hand corner of the grid square. The units
#' parameter controls the units (metres m or kilometres km) in which the coordinates
#' should be returned.
#'
#' @seealso
#' Function taken from [archived rnbn package](https://github.com/ropensci-archive/rnbn/issues/37).
#'
#' @author Stuart Ball.
#'
#' @param grid_reference A Great British or Irish grid reference character string with class gridref.
#' @param units Should the returned coordinates be in metres (m) or kilometres (km).
#'
#' @return A list of class gridref with the following contents:
#' * grid_reference: the original grid reference.
#' * system:	the grid reference projection system, either "OSGB" or "OSNI".
#' * x: the easting coordinate in requested units.
#' * y: the northing coordinate in requested units.
#' * units: the requested units either metres (m) or kilometres (km).
#' * precision: the precision of the original grid reference in metres
#' @keywords internal
gridCoords <- function(x, ...) {
  UseMethod("gridCoords", x)
}

#' @title
#' Manipulate OSGB or OSNI grid reference string
#'
#' @description
#' Extracts grid reference strings at various precisions from the supplied grid
#' reference string when possible. For example supplying a 1km square reference TL2998,
#' will return the 10km square TL29 but not a 100m square grid reference.
#'
#' @details
#' Tetrads are 2x2km squares and are often used for mapping distributions at the
#' scale of a county or similar sized local area. They are labelled using the 10km
#' square followed by a single, upper-case letter. Since there are 25 tetrads in a
#' 10km square, the letter "O" is not used to avoid confusion with zero. This is
#' named the DINTY system after the letters in the second row of this table.
#'
#' Pentads are 5x5km squares used for mapping at a regional scale. They are labelled
#' using the name of the 10km square followed by two upper-case letters.
#'
#' @inherit gridCoords return seealso
#'
#' @author Stuart Ball.
#'
#' @param grid_reference A Great British or Irish grid reference character string with class gridref
#' @param format The required grid reference format, either sq10km, sq5km, tetrad,
#' sq1km, sq100m or sq10m.
#'
#' @return The grid reference string formatted as requested.
#' @keywords internal
gridRef <- function(x) {
  UseMethod("gridRef", x)
}

#' @title
#' Get the precision of a grid reference
#'
#' @description
#' This function returns the grid reference's precision in metres.
#'
#' @details
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision).
#'
#' @section REST API:
#' A REST API endpoint is available once a Plumber router has been created and can be
#' requested by adding **_api** to the end of the function.
#'
#' The API returns "" instead of NULL so that it may be used with the dplyr::mutate function.
#' Wrap the API call with dplyr::na_if to convert "" to NA.
#'
#' ```
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(plumber)
#'   })
#' })
#' pr("../R/gridref_api.R") %>%
#'   pr_run(port = 8000)
#' ```
#'
#' @seealso
#' The function calls the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' @family grid reference functions
#'
#' @param grid_reference A Great British or Irish grid reference character string with class gridref.
#'
#' @return The precision of the grid reference in metres.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add precision column
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(precision = precision(grid_reference))
#'
precision <- function(x) {
  UseMethod("precision", x)
}

#' @title
#' Get the projection of a grid reference
#'
#' @description
#' This function returns the grid reference's projection, either as OSGB or OSNI.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#'
#' @return The projection of the grid reference in British National Grid (OSGB) or Irish National Grid (OSNI).
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add projection column
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(projection = projection(grid_reference))
projection <- function(x) {
  UseMethod("projection", x)
}

#' @title
#' Get the easting of a grid reference
#'
#' @description
#' This function returns the grid reference's easting in metres.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#' @param centre Should the easting be for the lower left hand corner (default FALSE) or the centre point (TRUE).
#'
#' @return The easting of grid reference in metres.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add easting column for the centre point
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(easting_corner = easting(grid_reference),
#'          easting_centre = easting(grid_reference, centre = TRUE))
easting <- function(x, ...) {
  UseMethod("easting", x)
}

#' @title
#' Get the northing of a grid reference
#'
#' @description
#' This function returns the grid reference's northing in metres.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#' @param centre Should the northing be for the lower left hand corner (default FALSE) or the centre point (TRUE).
#'
#' @return The northing of grid reference in metres.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add northing column for the centre point
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(northing_corner = northing(grid_reference),
#'          northing_centre = northing(grid_reference, centre = TRUE))
northing <- function(x, ...) {
  UseMethod("northing", x)
}

#' @title
#' Get the 10km grid reference
#'
#' @description
#' This function returns the 10km grid reference for a higher precision grid reference.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#'
#' @return The grid reference at 10km resolution.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add ten_km column
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(ten_km = hectad(grid_reference))
hectad <- function(x) {
  UseMethod("hectad", x)
}

#' @title
#' Get the 5km grid reference
#'
#' @description
#' This function returns the 5km grid reference for a higher precision grid reference.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#'
#' @return The grid reference at 5km resolution.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add five_km column
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(five_km = pentad(grid_reference))
pentad <- function(x) {
  UseMethod("pentad", x)
}

#' @title
#' Get the 2km grid reference
#'
#' @description
#' This function returns the 2km grid reference for a higher precision grid reference.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#'
#' @return The grid reference at 2km resolution.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add two_km column
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(two_km = tetrad(grid_reference))
tetrad <- function(x) {
  UseMethod("tetrad", x)
}

#' @title
#' Get the 1km grid reference
#'
#' @description
#' This function returns the 1km grid reference for a higher precision grid reference.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#'
#' @return The grid reference at 2km resolution.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add one_km column
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(one_km = monad(grid_reference))
monad <- function(x) {
  UseMethod("monad", x)
}

#' @title
#' Get the 100m grid reference
#'
#' @description
#' This function returns the 100m grid reference for a higher precision grid reference.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#'
#' @return The grid reference at 100m resolution.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add one_hundred_m column
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(one_hundred_m = hectare(grid_reference))
hectare <- function(x) {
  UseMethod("hectare", x)
}

#' @title
#' Get the 10m grid reference
#'
#' @description
#' This function returns the 10m grid reference for a higher precision grid reference.
#'
#' @inherit precision return details
#'
#' @inheritSection precision REST API
#'
#' @inherit precision return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#'
#' @return The grid reference at 10m resolution.
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # add ten_m column
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(ten_m = ten_metre(grid_reference))
ten_metre <- function(x) {
  UseMethod("ten_metre", x)
}

#' @title
#' Convert a OSGB or OSNI grid reference to a polygon geometry feature
#'
#' @description
#' This function converts a grid reference to its square polygon geometry feature
#' through conversion to well-known text.
#'
#' @inherit precision return details
#'
#' @inherit gridCoords return seealso
#'
#' @family grid reference functions
#'
#' @inheritParams precision
#'
#' @return The square polygon geometry feature
#'
#' @export
#'
#' @example man/examples/grid_references.R
#'
#' @examples
#' # create sf data frame
#' suppressPackageStartupMessages({
#'   suppressWarnings({
#'     library(dplyr)
#'     library(sf)
#'   })
#' })
#'
#' grid_references %>%
#'   rowwise() %>%
#'   mutate(geometry = gridsquare_geometry(grid_reference)) %>%
#'   st_as_sf()
gridsquare_geometry <- function(x) {
  UseMethod("gridsquare_geometry", x)
}

# S3 method functions ----------------------------------------------------------

#' @noRd
gridCoords.gridref <-  function (grid_reference, units = c("km", "m")) {
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
  gr <- toupper(gsub(" ", "", grid_reference))
  v <- regexec("^([H,N,O,S,T][A-H,J-Z]|[B-D,F-J,L-O,Q-T,V-X])([0-9]{2,10})([A-N,P-Z]{0,1})$",
               gr)
  letters <- unlist(regmatches(gr, v))[2]
  nums <- unlist(regmatches(gr, v))[3]
  tetrad <- unlist(regmatches(gr, v))[4]
  n <- nchar(nums)
  n <- n%/%2
  precision <- 10^(5 - n)
  gridref <- list()
  gridref$grid_reference <- grid_reference
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

#' @noRd
gridRef.gridref <- function(format){

  function(grid_reference) {

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

    gr <- toupper(gsub(" ", "", grid_reference))
    v <- regexec("^([H,N,O,S,T][A-H,J-Z]|[B-D,F-J,L-O,Q-T,V-X])([0-9]{2,10})([A-N,P-Z]{0,1})$",
                 gr)
    letters <- unlist(regmatches(gr, v))[2]
    nums <- unlist(regmatches(gr, v))[3]
    tetrad <- unlist(regmatches(gr, v))[4]
    n <- nchar(nums)
    n <- n%/%2
    gret <- list()
    gret$grid_reference <- grid_reference
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
    })

    return(gret[["gfmt"]])
  }
}

#' @rdname precision
#' @export
precision.gridref <- function(grid_reference) {

  gridCoords(grid_reference, units = "m") %>%
    purrr::pluck("precision")
}

#' @rdname projection
#' @export
projection.gridref <- function(grid_reference) {

  gridCoords(grid_reference) %>%
    purrr::pluck("system")
}

#' @rdname easting
#' @export
easting.gridref <- function(grid_reference, centre = FALSE) {

  # Get easting for lower left hand corner using rNBN
  easting <- gridCoords(grid_reference, units = "m") %>%
    purrr::pluck("x")

  # get easting for centre point
  if(centre) {

    # Get precision using rNBN
    precision <- gridCoords(grid_reference, units = "m") %>%
      purrr::pluck("precision")

    easting <- easting + (precision / 2L)
  }

  return(easting)
}

#' @rdname northing
#' @export
northing.gridref <- function(grid_reference, centre = FALSE) {

  # Get northing for lower left hand corner using rNBN
  northing <- gridCoords(grid_reference, units = "m") %>%
    purrr::pluck("y")

  # get northing for centre point
  if(centre) {

    # Get precision using rNBN
    precision <- gridCoords(grid_reference, units = "m") %>%
      purrr::pluck("precision")

    northing <- northing + (precision / 2L)
  }

  return(northing)
}

#' @rdname hectad
#' @export
hectad.gridref <- gridRef.gridref(format = "sq10km")

#' @rdname pentad
#' @export
pentad.gridref <- gridRef.gridref(format = "sq5km")

#' @rdname tetrad
#' @export
tetrad.gridref <- gridRef.gridref(format = "tetrad")

#' @rdname monad
#' @export
monad.gridref <- gridRef.gridref(format = "sq1km")

#' @rdname hectare
#' @export
hectare.gridref <- gridRef.gridref(format = "sq100m")

#' @rdname ten_metre
#' @export
ten_metre.gridref <- gridRef.gridref(format = "sq10m")

#' @rdname gridsquare_geometry
#' @export
gridsquare_geometry.gridref <- function(grid_reference) {

  coords <- gridCoords(grid_reference, unit = "m")
  easting <- coords %>% purrr::pluck("x")
  northing <- coords %>% purrr::pluck("y")
  precision <- coords %>% purrr::pluck("precision")
  projection <- coords %>% purrr::pluck("system")

  # Get EPSG code
  if(projection == "OSGB") {
    epsg = 27700
  } else if (projection == "OSNI") {
    epsg = 29902
  }

  # convert coordinates to WKT
  wkt <- stringr::str_glue("POLYGON (({easting} {northing}, {easting + precision} {northing}, {easting + precision} {northing + precision}, {easting} {northing + precision}, {easting} {northing}))") %>%
    vctrs::vec_cast(character())

  # convert to geometry feature
  sf::st_as_sfc(wkt, crs = epsg)
}

# gridref_api ##################################################################

#' @noRd
gridref_api <- function(gridref_method) {

  function(grid_reference, centre = FALSE) {

    # api request
    url_address <- "http://127.0.0.1:8000"
    request <- stringr::str_glue("{url_address}/{gridref_method}?grid_reference={grid_reference}")

    # add centre parameter to api request
    if (gridref_method %in% c("easting", "northing")) {
      request <- stringr::str_glue("{request}&centre={centre}")
    }

    # api response
    response <- httr::GET(request) %>%
      httr::stop_for_status() %>%
      httr::content() %>%
      unlist()
  }
}

#' @title precision_api
#' @inherit precision
#' @export
#' @keywords internal
precision_api <- gridref_api(gridref_method = "precision")

#' @title projection_api
#' @inherit projection
#' @export
#' @keywords internal
projection_api <- gridref_api(gridref_method = "projection")

#' @title easting_api
#' @inherit easting
#' @export
#' @keywords internal
easting_api <- gridref_api(gridref_method = "easting")

#' @title northing_api
#' @inherit northing
#' @export
#' @keywords internal
northing_api <- gridref_api(gridref_method = "northing")

#' @title hectad_api
#' @inherit hectad
#' @export
#' @keywords internal
hectad_api <- gridref_api(gridref_method = "hectad")

#' @title pentad_api
#' @inherit pentad
#' @export
#' @keywords internal
pentad_api <- gridref_api(gridref_method = "pentad")

#' @title tetrad_api
#' @inherit tetrad
#' @export
#' @keywords internal
tetrad_api <- gridref_api(gridref_method = "tetrad")

#' @title monad_api
#' @inherit monad
#' @export
#' @keywords internal
monad_api <- gridref_api(gridref_method = "monad")

#' @title hectare_api
#' @inherit hectare
#' @export
#' @keywords internal
hectare_api <- gridref_api(gridref_method = "hectare")

#' @title ten_metre_api
#' @inherit ten_metre
#' @export
#' @keywords internal
ten_metre_api <- gridref_api(gridref_method = "ten_metre")
