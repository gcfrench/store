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
#'   dplyr::mutate(grid_reference = as_gridref(grid_reference)) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(projection = projection(grid_reference))
#'}
projection.gridref <- function(grid_reference) {

  gridCoords(grid_reference) %>%
    purrr::pluck("system")
}
