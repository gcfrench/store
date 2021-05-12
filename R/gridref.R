#' Convert to gridref object
#'
#' @description
#' Convert character string to gridref object
#'
#' @seealso
#' Constructor function for gridref class as described in [S3](https://adv-r.hadley.nz/s3.html)
#' chapter of Advanced R second additon by Hadley Wickham
#'
#' @param x character, grid reference string
#'
#' @return x, grid reference string with class gridref
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'})
#'
#' # convert to gridref class
#' nbn_demonstration_dataset %>%
#'   janitor::clean_names() %>%
#'   dplyr::select(grid_reference) %>%
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference))
#'}
as_gridref <- function(x = character()) {

  # gridref must be a character string
  stopifnot(is.character(x))

  # gridref must be a valid grid reference
  validate_gridref(x)

  # convert to gridref class
  structure(x, class = "gridref")
}

# validator function
#' Title
#'
#' @description
#' Validator function to check that grid reference is valid. Taken from
#' [archived rnbn package](https://github.com/ropensci-archive/rnbn/issues/37).
#' Error returned if invalid
#'
#' @author Stuart Ball
#'
#' @param grid character, grid reference string to validate
#'
#' @return grid, validated grid reference string
validate_gridref <- function(grid) {

  gr <- toupper(gsub(" ", "", grid))
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

  return(grid)
}

#' Generic function for gridCoords
#'
#' Generic function for gridCoords as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by function gridCoords
#'
#' @return
gridCoords <- function(x, ...) {
  UseMethod("gridCoords", x)
}

#' Generic function for precision
#'
#' Generic function for precision as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function precision
#'
#' @return
#' @export
precision <- function(x, ...) {
  UseMethod("precision", x)
}

#' Generic function for projection
#'
#' Generic function for projection as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function projection
#'
#' @return
#' @export
projection <- function(x, ...) {
  UseMethod("projection", x)
}

#' Generic function for easting
#'
#' Generic function for easting as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function easting
#'
#' @return
#' @export
easting <- function(x, ...) {
  UseMethod("easting", x)
}

#' Generic function for northing
#'
#' Generic function for northing as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function northing
#'
#' @return
#' @export
northing <- function(x, ...) {
  UseMethod("northing", x)
}

#' Generic function for gridRef
#'
#' Generic function for gridRef as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by function gridRef
#'
#' @return
gridRef <- function(x, ...) {
  UseMethod("gridRef", x)
}

#' Generic function for hectad
#'
#' Generic function for hectad as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function hectad
#'
#' @return
#' @export
hectad <- function(x, ...) {
  UseMethod("hectad", x)
}

#' Generic function for pentad
#'
#' Generic function for hectad as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function pentad
#'
#' @return
#' @export
pentad <- function(x, ...) {
  UseMethod("pentad", x)
}

#' Generic function for tetrad
#'
#' Generic function for hectad as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function tetrad
#'
#' @return
#' @export
tetrad <- function(x, ...) {
  UseMethod("tetrad", x)
}

#' Generic function for monad
#'
#' Generic function for monad as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function monad
#'
#' @return
#' @export
monad <- function(x, ...) {
  UseMethod("monad", x)
}

#' Generic function for hectare
#'
#' Generic function for hectare as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function hectare
#'
#' @return
#' @export
hectare <- function(x, ...) {
  UseMethod("hectare", x)
}

#' Generic function for gridsquare_geometry
#'
#' Generic function for gridsquare_geometry as described in [The S3 Object system](http://adv-r.had.co.nz/S3.html)
#' chapter of Advanced R by Hadley Wickham
#'
#' @param x object, contains class for method dispatch by generic function gridsquare_geometry
#'
#' @return
#' @export
gridsquare_geometry <- function(x, ...) {
  UseMethod("gridsquare_geometry", x)
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
gridCoords.gridref <-  function (grid, units = c("km", "m")) {
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
  letters <- unlist(regmatches(gr, v))[2]
  nums <- unlist(regmatches(gr, v))[3]
  tetrad <- unlist(regmatches(gr, v))[4]
  n <- nchar(nums)
  n <- n%/%2
  precision <- 10^(5 - n)
  gridref <- list()
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
gridRef.gridref <- function(format){

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
    letters <- unlist(regmatches(gr, v))[2]
    nums <- unlist(regmatches(gr, v))[3]
    tetrad <- unlist(regmatches(gr, v))[4]
    n <- nchar(nums)
    n <- n%/%2
    gret <- list()
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
    })

    return(gret[["gfmt"]])
  }
}

#' Get precision for grid reference
#'
#' @description
#' This function returns the grid reference's precision in metres.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(precision = precision(grid_reference))
#'}
precision.gridref <- function(grid_reference) {

  gridCoords(grid_reference, units = "m") %>%
    purrr::pluck("precision")
}


#' Get projection for grid reference
#'
#' This function returns the grid reference's projection, either as OSGB or OSNI.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(projection = projection(grid_reference))
#'}
projection.gridref <- function(grid_reference) {

  gridCoords(grid_reference) %>%
    purrr::pluck("system")
}

#' Get easting for grid reference
#'
#' This function returns the grid reference's easting in metres.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(easting = easting(grid_reference, centre = FALSE))
#'}
easting.gridref <- function(grid_reference, centre = FALSE) {

  # Get easting for lower left hand corner using rNBN
  easting <- gridCoords(grid_reference, units = "m") %>%
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
#' This function returns the grid reference's northing in metres.
#' It uses the gridCoords function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can check either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(northing = northing(grid_reference, centre = FALSE))
#'}
northing.gridref <- function(grid_reference, centre = FALSE) {

  # Get northing for lower left hand corner using rNBN
  northing <- gridCoords(grid_reference, units = "m") %>%
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

#' Get 10km grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(ten_km = hectad(grid_reference))
#'}
hectad.gridref <- gridRef.gridref(format = "sq10km")

#' Get 5km grid reference
#'
#' This function returns the 5km grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' but does not convert tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(five_km = pentad(grid_reference))
#'}
pentad.gridref <- gridRef.gridref(format = "sq5km")

#' Get 2km grid reference
#'
#' This function returns the 2km grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(two_km = tetrad(grid_reference))
#'}
tetrad.gridref <- gridRef.gridref(format = "tetrad")

#' Get 1km grid reference
#'
#' This function returns the 1km grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' but does not convert tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(one_km = monad(grid_reference))
#'}
monad.gridref <- gridRef.gridref(format = "sq1km")

#' Get 100m grid reference
#'
#' This function returns the 100m grid reference for a higher precision grid reference.
#' It uses the gridRef function in the archived [rnbn](https://github.com/ropensci-archive/rnbn/issues/37) package.
#'
#' It can convert either British or Irish grid references up to 10 figure (1m precision),
#' including tetrads (2000m precision)
#'
#' @family grid reference functions
#'
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(one_hundred_m = hectare(grid_reference))
#'}
hectare.gridref <- gridRef.gridref(format = "sq100m")

#' Convert OSGB or OSNI Grid reference to polygon geometry feature
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
#' @param grid_reference character gridref class, British or Irish grid reference
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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(geometry = gridsquare_geometry(grid_reference)) %>%
#'   sf::st_as_sf() %>%
#'   sf::st_write("output/nbn_demonstration_dataset.shp")
#'}
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


