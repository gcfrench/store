#' @title
#' example_main_polygon
#'
#' @description
#' One KM square polygon in British National Grid projection.
#'
#' @section Formats:
#' The spatial object can be saved in parquet and shape file format by running the code in
#' the example_data. R script stored in the data_raw folder. The files are saved
#' in the inst/extdata directory.
#'
#' @format
#' A spatial object with `r nrow(example_main_polygon)` attribute row and `r ncol(example_main_polygon)` columns.
#' \describe{
#'   \item{id}{integer id of polygon.}
#' }
"example_main_polygon"

#' @title
#' example_polygons
#'
#' @description
#' Four 500m square polygons in British National Grid projection. These polygons
#' either overlap, are contain within, touches or are outside the example_main_polygon.
#'
#' @inheritSection example_main_polygon Formats
#'
#' @format
#' A spatial object with `r nrow(example_polygons)` attribute rows and `r ncol(example_polygons)` columns.
#' \describe{
#'   \item{id}{integer id of polygon.}
#'   \item{relation}{spatial relationship of polygon to the example main polygon.}
#'   \item{DE-9IM}{The Dimensionally Extended 9-Intersection Model used to describe the
#'   spatial relationship between the example polygon and the example main polygon.}
#' }
"example_polygons"

#' @title
#' example_multipolygon
#'
#' @description
#' One multipolygon created from the four example polygons in British National Grid
#' projection, using st_union to resolve the polygon boundaries.
#'
#' @inheritSection example_main_polygon Formats
#'
#' @format
#' A spatial object with `r nrow(example_multipolygon)` attribute rows and `r ncol(example_multipolygon)` columns.
#' \describe{
#'   \item{FID}{integer id of multipolygon.}
#' }
"example_multipolygon"

#' @title
#' example_points
#'
#' @description
#' Twenty points, consisting of five rows of four, in British National Grid projection.
#' These points either are contain within, touches or are outside the example_main_polygon.
#'
#' @inheritSection example_main_polygon Formats
#'
#' @format
#' A spatial object with `r nrow(example_points)` attribute rows and `r ncol(example_points)` columns.
#' \describe{
#'   \item{id}{integer id of multipolygon.}
#'   \item{relation}{spatial relationship of points to the example main polygon.}
#' }
"example_points"

#' @title
#' example_multipoint
#'
#' @description
#' One multipoint created from the twenty example points in British National Grid
#' projection.
#'
#' @inheritSection example_main_polygon Formats
#'
#' @format
#' A spatial object with `r nrow(example_multipoint)` attribute rows and `r ncol(example_multipoint)` columns.
#' \describe{
#'   \item{FID}{integer id of multipoint.}
#' }
"example_multipoint"

#' @title
#' example_lines
#'
#' @description
#' Five parallel lines in British National Grid projection. These lines either
#' overlaps, touches or are outside the example_main_polygon.
#'
#' @inheritSection example_main_polygon Formats
#'
#' @format
#' A spatial object with `r nrow(example_lines)` attribute rows and `r ncol(example_lines)` columns.
#' \describe{
#'   \item{id}{integer id of linestring.}
#'   \item{relation}{spatial relationship of linestrings to the example main polygon.}
#' }
"example_lines"

#' @title
#' example_multiline
#'
#' @description
#' One multiline created from the five linestrings in British National Grid
#' projection.
#'
#' @inheritSection example_main_polygon Formats
#'
#' @format
#' A spatial object with `r nrow(example_multiline)` attribute rows and `r ncol(example_multiline)` columns.
#' \describe{
#'   \item{FID}{integer id of multiline.}
#' }
"example_multiline"

#' @title
#' example_raster
#'
#' @description
#' One raster image consisting of 30 rows and 45 columns in British National Grid
#' projection. This image contains one layer with cells values from 1 to 1350 start
#' in the top left hand cell.
#'
#' @section Formats:
#' The raster images is saved as a tif file in the inst/extdata/tif_files directory
#'
#' @examples
#' \dontrun{
#' raster_path <- system.file("extdata", "tif_files", "example_raster.tif", package = "spatialstore")
#  terra::rast(raster_path)
#' }
