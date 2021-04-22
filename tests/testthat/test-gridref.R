# validate_gridref
test_that("valid OSGB hectad grid reference", {
  expect_equal(validate_gridref("TL28"), "TL28")
})

test_that("valid OSGB tetrad grid reference", {
  expect_equal(validate_gridref("TL28A"), "TL28A")
})

test_that("valid OSGB monad grid reference", {
  expect_equal(validate_gridref("TL2181"), "TL2181")
})

test_that("valid OSGB hectare grid reference", {
  expect_equal(validate_gridref("TL211811"), "TL211811")
})

test_that("valid OSGB 10m grid reference", {
  expect_equal(validate_gridref("TL21118111"), "TL21118111")
})

test_that("valid OSGB 1m grid reference", {
  expect_equal(validate_gridref("TL2111181111"), "TL2111181111")
})

test_that("valid OSNI hectad grid reference", {
  expect_equal(validate_gridref("T28"), "T28")
})

test_that("valid OSNI tetrad grid reference", {
  expect_equal(validate_gridref("T28A"), "T28A")
})

test_that("valid OSNI monad grid reference", {
  expect_equal(validate_gridref("T2181"), "T2181")
})

test_that("valid OSNI hectare grid reference", {
  expect_equal(validate_gridref("T211811"), "T211811")
})

test_that("valid OSNI 10m grid reference", {
  expect_equal(validate_gridref("T21118111"), "T21118111")
})

test_that("valid OSNI 1m grid reference", {
  expect_equal(validate_gridref("T2111181111"), "T2111181111")
})

test_that("invalid OSGB gridreference", {
  expect_error(validate_gridref("TLL28"))
})

test_that("invalid OSGB gridreference", {
  expect_error(validate_gridref("TL281"))
})

test_that("invalid OSNI gridreference", {
  expect_error(validate_gridref("O281"))
})

test_that("invalid OSNI gridreference", {
  expect_error(validate_gridref("T281"))
})

# precision
test_that("OSGB hectad precision", {
  expect_equal(precision.gridref("TL28"), 10000)
})

test_that("OSGB tetrad precision", {
  expect_equal(precision.gridref("TL28A"), 2000)
})

test_that("OSGB monad precision", {
  expect_equal(precision.gridref("TL2181"), 1000)
})

test_that("OSGB hectare precision", {
  expect_equal(precision.gridref("TL211811"), 100)
})

test_that("OSGB 10m precision", {
  expect_equal(precision.gridref("TL21118111"), 10)
})

test_that("OSGB 1m precision", {
  expect_equal(precision.gridref("TL2111181111"), 1)
})

test_that("OSNI hectad precision", {
  expect_equal(precision.gridref("T28"), 10000)
})

test_that("OSNI tetrad precision", {
  expect_equal(precision.gridref("T28A"), 2000)
})

test_that("OSNI monad precision", {
  expect_equal(precision.gridref("T2181"), 1000)
})

test_that("OSNI hectare precision", {
  expect_equal(precision.gridref("T211811"), 100)
})

test_that("OSNI 10m precision", {
  expect_equal(precision.gridref("T21118111"), 10)
})

test_that("OSNI 1m precision", {
  expect_equal(precision.gridref("T2111181111"), 1)
})

# projection
test_that("OSGB hectad projection", {
  expect_equal(projection.gridref("TL28"), "OSGB")
})

test_that("OSGB tetrad projection", {
  expect_equal(projection.gridref("TL28A"), "OSGB")
})

test_that("OSGB monad projection", {
  expect_equal(projection.gridref("TL2181"), "OSGB")
})

test_that("OSGB hectare projection", {
  expect_equal(projection.gridref("TL211811"), "OSGB")
})

test_that("OSGB 10m projection", {
  expect_equal(projection.gridref("TL21118111"), "OSGB")
})

test_that("OSGB 1m projection", {
  expect_equal(projection.gridref("TL2111181111"), "OSGB")
})

test_that("OSNI hectad projection", {
  expect_equal(projection.gridref("T28"), "OSNI")
})

test_that("OSNI tetrad projection", {
  expect_equal(projection.gridref("T28A"), "OSNI")
})

test_that("OSNI monad projection", {
  expect_equal(projection.gridref("T2181"), "OSNI")
})

test_that("OSNI hectare projection", {
  expect_equal(projection.gridref("T211811"), "OSNI")
})

test_that("OSNI 10m projection", {
  expect_equal(projection.gridref("T21118111"), "OSNI")
})

test_that("OSNI 1m projection", {
  expect_equal(projection.gridref("T2111181111"), "OSNI")
})

