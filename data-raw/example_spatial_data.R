# example_main_polygon ---------------------------------------------------------
## One polygon with 1km^2^ area

### geometry
polygon_geometry <- list(rbind(c(250, 250), c(250, 1250), c(1250, 1250), c(1250, 250), c(250, 250))) |>
  sf::st_polygon() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 1L
)

example_main_polygon <- sf::st_sf(attribute_fields, geometry = polygon_geometry)

# save in data directory
example_main_polygon |>
  usethis::use_data(overwrite = TRUE)

# ------------------------------------------------------------------------------

# example_polygons -------------------------------------------------------------
## 4 polygons, overlapping, contained within, touching and outside example main polygon,
## resulting in intersection area 0.38 km^2^.

## example_polygon_1
### geometry
polygon_geometry <- list(rbind(c(0, 500), c(0, 1000), c(500, 1000), c(500, 500), c(0, 500))) |>
  sf::st_polygon() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 1L,
  relation = "overlaps",
  `DE-9IM` = "212101212",
)

example_polygon_1 <- sf::st_sf(attribute_fields, geometry = polygon_geometry)

## example_polygon_2
### geometry
polygon_geometry <- list(rbind(c(500, 500), c(500, 1000), c(1000, 1000), c(1000, 500), c(500, 500))) |>
  sf::st_polygon() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 2L,
  relation = "contains",
  `DE-9IM` = "2FF1FF212"
)

example_polygon_2 <- sf::st_sf(attribute_fields, geometry = polygon_geometry)

## example_polygon_3
### geometry
polygon_geometry <- list(rbind(c(1250, 500), c(1250, 1000), c(1750, 1000), c(1750, 500), c(1250, 500))) |>
  sf::st_polygon() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 3L,
  relation = "touches",
  `DE-9IM` = "FF2F11212"
)

example_polygon_3 <- sf::st_sf(attribute_fields, geometry = polygon_geometry)

## example_polygon_4
### geometry
polygon_geometry <- list(rbind(c(1750, 500), c(1750, 1000), c(2250, 1000), c(2250, 500), c(1750, 500))) |>
  sf::st_polygon() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 4L,
  relation = "outside",
  `DE-9IM` = "FF2FF1212"
)

example_polygon_4 <- sf::st_sf(attribute_fields, geometry = polygon_geometry)

# combine polygons into single layer
example_polygons <- dplyr::bind_rows(example_polygon_1, example_polygon_2, example_polygon_3, example_polygon_4)

# save in data directory
example_polygons |>
  usethis::use_data(overwrite = TRUE)

# ------------------------------------------------------------------------------

# example_multi-polygons -------------------------------------------------------
## four example polygons combined as one multi-polygon, resulting in intersection area 0.38 km^2^.
example_multipolygon <- example_polygons |>
  sf::st_union() |>   # sf::st_combine preserves boundaries
  sf::st_as_sf()
example_multipolygon  |>
  usethis::use_data(overwrite = TRUE)

# ------------------------------------------------------------------------------

# example_points ------------------------------------------------------------------
## 20 points, resulting in 6 points intersecting main polygon: 5 touches, 1 contained within
example_points <- tibble::tibble(
  id = 1:20,
  x = rep(c(250, 750, 1500, 2000), 5),
  y = rep(c(0, 250, 750, 1250, 1500), 4),
  relation = c("outside", "touches", "outside", "outside", "outside",
               "outside", "outside", "outside", "touches", "outside",
               "outside", "outside", "touches", "touches", "outside",
               "outside", "touches", "contains", "outside", "outside")
) |>
  sf::st_as_sf(coords = c("x", "y")) |>
  sf::st_set_crs("EPSG:27700")

# save in data directory
example_points |>
  usethis::use_data(overwrite = TRUE)

# ------------------------------------------------------------------------------

# example_multipoints ----------------------------------------------------------
## 20 points combined as 1 multipoint, resulting in 6 points intersecting with main polygon
example_multipoint <- example_points |>
  sf::st_union() |>
  sf::st_as_sf()
example_multipoint |>
  usethis::use_data(overwrite = TRUE)

# ------------------------------------------------------------------------------

# example_lines ----------------------------------------------------------------
## 5 lines, resulting in 3 lines intersecting with main polygon, with length of 3km.

## example_line_1
### geometry
line_geometry <- rbind(c(0, 0), c(2250, 0)) |>
  sf::st_linestring() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 1L,
  relation = "outside"
)

example_line_1 <- sf::st_sf(attribute_fields, geometry = line_geometry)

## example_line_2
### geometry
line_geometry <- rbind(c(0, 250), c(2250, 250)) |>
  sf::st_linestring() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 2L,
  relation = "touches"
)

example_line_2 <- sf::st_sf(attribute_fields, geometry = line_geometry)

## example_line_3
### geometry
line_geometry <- rbind(c(0, 750), c(2250, 750)) |>
  sf::st_linestring() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 3L,
  relation = "overlaps"
)

example_line_3 <- sf::st_sf(attribute_fields, geometry = line_geometry)

## example_line_4
### geometry
line_geometry <- rbind(c(0, 1250), c(2250, 1250)) |>
  sf::st_linestring() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 4L,
  relation = "touches"
)

example_line_4 <- sf::st_sf(attribute_fields, geometry = line_geometry)

## example_line_5
### geometry
line_geometry <- rbind(c(0, 1500), c(2250, 1500)) |>
  sf::st_linestring() |>
  sf::st_sfc(crs = "EPSG:27700")

### attribute field
attribute_fields <- tibble::tibble(
  id = 5L,
  relation = "outside"
)

example_line_5 <- sf::st_sf(attribute_fields, geometry = line_geometry)

### combine lines into single layer
example_lines <- dplyr::bind_rows(example_line_1, example_line_2, example_line_3, example_line_4, example_line_5)

# save in data directory
example_lines |>
  usethis::use_data(overwrite = TRUE)

# ------------------------------------------------------------------------------

# example_multilines -----------------------------------------------------------
## 5 lines combines as 1 multiline, resulting in 3 lines intersecting with main polygon, with length of 3km.
example_multiline <- example_lines|>
  sf::st_union() |>
  sf::st_as_sf()
example_multiline |>
  usethis::use_data(overwrite = TRUE)

# ------------------------------------------------------------------------------

# example_raster ---------------------------------------------------------------
## 1 raster with 30 rows and 45 columns, with resolution of 50m.
example_raster <- terra::rast(nrows = 30, ncols = 45,
                              xmin = 0, xmax = 2250, ymin = 0, ymax = 1500,
                              crs = "EPSG:27700",
                              vals = 1:1350,
                              nlyrs = 1, names = c("layer_1"))

## save example raster layer
example_raster |>
  terra::writeRaster(fs::path("inst", "extdata", "tif_files", "example_raster.tif"))
# ------------------------------------------------------------------------------

# save rda files as parquet and shape files ------------------------------------
library(spatialstore)

## create directories
fs::dir_create(fs::path("inst", "extdata", "parquet_files"))
fs::dir_create(fs::path("inst", "extdata", "shape_files"))

# export parquet and shape files
fs::dir_ls(fs::path("data"), glob = "*.rda") |>
  fs::path_file() |>
  fs::path_ext_remove() |>
  purrr::walk(function(.x) {

    # file paths
    parquet_path <- fs::path("inst", "extdata", "parquet_files", glue::glue("{.x}.parquet"))
    shape_file_path <- fs::path("inst", "extdata", "shape_files", glue::glue("{.x}.shp"))

    # file name to symbol
    data <- rlang::sym(.x)

    # write parquet file
    sfarrow::st_write_parquet(rlang::eval_tidy(data), parquet_path, compression = "snappy")

    # write shape file
    sf::write_sf(rlang::eval_tidy(data), shape_file_path)
  })
# ------------------------------------------------------------------------------

# plot example spatial objects -------------------------------------------------
library(spatialstore)
example_raster <- terra::rast(fs::path("inst", "extdata", "tif_files", "example_raster.tif"))

tmap::tm_shape(example_raster) +
  tmap::tm_raster(palette = "Blues",
                  style = "cont") +
  tmap::tm_shape(example_main_polygon) +
  tmap::tm_polygons(id = "id",
                    col = "gray50",
                    border.col = "gray25",
                    lwd = 2) +
  tmap::tm_shape(example_polygons) +
  tmap::tm_polygons(id = "id",
                    col = "firebrick1",
                    border.col = "firebrick4",
                    lwd = 2) +
  tmap::tm_shape(example_lines) +
  tmap::tm_lines(id = "id",
                 col = "firebrick4",
                 lwd = 4) +
  tmap::tm_shape(example_points) +
  tmap::tm_dots(id = "id",
                col = "firebrick3",
                size = 0.2) +
  tmap::tm_layout(legend.show = FALSE,
                  frame = TRUE,
                  frame.lwd = 2,
                  inner.margins = c(bottom = 0.02, left = 0.01, top = 0.02, right = 0.01),
                  outer.margins = c(bottom = 0, left = 0.01, top = 0, right = 0.01),
                  outer.bg.color = "grey90")

# ------------------------------------------------------------------------------










