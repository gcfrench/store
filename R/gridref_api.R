#* @apiTitle Grid reference API
#* @apiDescription API for extracting and formatting grid reference information.

#* Gets a grid reference's precision in metres.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @get /precision
function(grid_reference) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::precision()
}

#* Gets a grid reference's projection.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @get /projection
function(grid_reference) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::projection()
}

#* Gets a grid reference's easting coordinate in metres.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @param centre Should the easting be the lower left hand corner (default FALSE) or the centre point (TRUE).
#* @get /easting
function(grid_reference, centre = FALSE) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::easting(centre = centre)
}

#* Gets a grid reference's northing coordinate in metres.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @param centre Should the easting be the lower left hand corner (default FALSE) or the centre point (TRUE).
#* @get /northing
function(grid_reference, centre = FALSE) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::northing(centre = centre)
}

#* Gets the 10km grid reference.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @get /hectad
function(grid_reference) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::hectad()
}

#* Gets the 5km grid reference.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @get /pentad
function(grid_reference) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::pentad()
}

#* Gets the 2km grid reference.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @get /tetrad
function(grid_reference) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::tetrad()
}

#* Gets the 1km grid reference.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @get /monad
function(grid_reference) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::monad()
}

#* Gets the 100m grid reference.
#* @param grid_reference A Great British or Irish grid reference character string.
#* @get /hectare
function(grid_reference) {
  grid_reference <- store::as_gridref(grid_reference) %>%
    store::hectare()
}
