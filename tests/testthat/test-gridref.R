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
