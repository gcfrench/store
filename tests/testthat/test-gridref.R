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
  expect_equal(precision(as_gridref("TL28")), 10000)
})

test_that("OSGB tetrad precision", {
  expect_equal(precision(as_gridref("TL28A")), 2000)
})

test_that("OSGB monad precision", {
  expect_equal(precision(as_gridref("TL2181")), 1000)
})

test_that("OSGB hectare precision", {
  expect_equal(precision(as_gridref("TL211811")), 100)
})

test_that("OSGB 10m precision", {
  expect_equal(precision(as_gridref("TL21118111")), 10)
})

test_that("OSGB 1m precision", {
  expect_equal(precision(as_gridref("TL2111181111")), 1)
})

test_that("OSNI hectad precision", {
  expect_equal(precision(as_gridref("T28")), 10000)
})

test_that("OSNI tetrad precision", {
  expect_equal(precision(as_gridref("T28A")), 2000)
})

test_that("OSNI monad precision", {
  expect_equal(precision(as_gridref("T2181")), 1000)
})

test_that("OSNI hectare precision", {
  expect_equal(precision(as_gridref("T211811")), 100)
})

test_that("OSNI 10m precision", {
  expect_equal(precision(as_gridref("T21118111")), 10)
})

test_that("OSNI 1m precision", {
  expect_equal(precision(as_gridref("T2111181111")), 1)
})

# projection
test_that("OSGB hectad projection", {
  expect_equal(projection(as_gridref("TL28")), "OSGB")
})

test_that("OSGB tetrad projection", {
  expect_equal(projection(as_gridref("TL28A")), "OSGB")
})

test_that("OSGB monad projection", {
  expect_equal(projection(as_gridref("TL2181")), "OSGB")
})

test_that("OSGB hectare projection", {
  expect_equal(projection(as_gridref("TL211811")), "OSGB")
})

test_that("OSGB 10m projection", {
  expect_equal(projection(as_gridref("TL21118111")), "OSGB")
})

test_that("OSGB 1m projection", {
  expect_equal(projection(as_gridref("TL2111181111")), "OSGB")
})

test_that("OSNI hectad projection", {
  expect_equal(projection(as_gridref("T28")), "OSNI")
})

test_that("OSNI tetrad projection", {
  expect_equal(projection(as_gridref("T28A")), "OSNI")
})

test_that("OSNI monad projection", {
  expect_equal(projection(as_gridref("T2181")), "OSNI")
})

test_that("OSNI hectare projection", {
  expect_equal(projection(as_gridref("T211811")), "OSNI")
})

test_that("OSNI 10m projection", {
  expect_equal(projection(as_gridref("T21118111")), "OSNI")
})

test_that("OSNI 1m projection", {
  expect_equal(projection(as_gridref("T2111181111")), "OSNI")
})

# easting
## OSGB lower left corner
test_that("OSGB hectad grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("TL28")), 520000)
})

test_that("OSGB tetrad grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("TL28A")), 520000)
})

test_that("OSGB monad grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("TL2181")), 521000)
})

test_that("OSGB hectare grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("TL211811")), 521100)
})

test_that("OSGB 10m grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("TL21118111")), 521110)
})

test_that("OSGB 1m grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("TL2111181111")), 521111)
})

## OSGB centre
test_that("OSGB hectad grid reference easting centre", {
  expect_equal(easting(as_gridref("TL28"), centre = TRUE), 525000)
})

test_that("OSGB tetrad grid reference easting centre", {
  expect_equal(easting(as_gridref("TL28A"), centre = TRUE), 521000)
})

test_that("OSGB monad grid reference easting centre", {
  expect_equal(easting(as_gridref("TL2181"), centre = TRUE), 521500)
})

test_that("OSGB hectare grid reference easting centre", {
  expect_equal(easting(as_gridref("TL211811"), centre = TRUE), 521150)
})

test_that("OSGB 10m grid reference easting centre", {
  expect_equal(easting(as_gridref("TL21118111"), centre = TRUE), 521115)
})

test_that("OSGB 1m grid reference easting lower centre", {
  expect_equal(easting(as_gridref("TL2111181111"), centre = TRUE), 521111.5)
})

## OSNI lower left corner
test_that("OSNI hectad grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("T28")), 320000)
})

test_that("OSNI tetrad grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("T28A")), 320000)
})

test_that("OSGB monad grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("T2181")), 321000)
})

test_that("OSNI hectare grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("T211811")), 321100)
})

test_that("OSNI 10m grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("T21118111")), 321110)
})

test_that("OSNI 1m grid reference easting lower left corner", {
  expect_equal(easting(as_gridref("T2111181111")), 321111)
})

## OSNIcentre
test_that("OSGB hectad grid reference easting centre", {
  expect_equal(easting(as_gridref("T28"), centre = TRUE), 325000)
})

test_that("OSGB tetrad grid reference easting centre", {
  expect_equal(easting(as_gridref("T28A"), centre = TRUE), 321000)
})

test_that("OSGB monad grid reference easting centre", {
  expect_equal(easting(as_gridref("T2181"), centre = TRUE), 321500)
})

test_that("OSGB hectare grid reference easting centre", {
  expect_equal(easting(as_gridref("T211811"), centre = TRUE), 321150)
})

test_that("OSGB 10m grid reference easting centre", {
  expect_equal(easting(as_gridref("T21118111"), centre = TRUE), 321115)
})

test_that("OSGB 1m grid reference easting lower centre", {
  expect_equal(easting(as_gridref("T2111181111"), centre = TRUE), 321111.5)
})

# northing
## OSGB lower left corner
test_that("OSGB hectad grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("TL28")), 280000)
})

test_that("OSGB tetrad grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("TL28A")), 280000)
})

test_that("OSGB monad grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("TL2181")), 281000)
})

test_that("OSGB hectare grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("TL211811")), 281100)
})

test_that("OSGB 10m grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("TL21118111")), 281110)
})

test_that("OSGB 1m grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("TL2111181111")), 281111)
})

## OSGB centre
test_that("OSGB hectad grid reference northing centre", {
  expect_equal(northing(as_gridref("TL28"), centre = TRUE), 285000)
})

test_that("OSGB tetrad grid reference northing centre", {
  expect_equal(northing(as_gridref("TL28A"), centre = TRUE), 281000)
})

test_that("OSGB monad grid reference northing centre", {
  expect_equal(northing(as_gridref("TL2181"), centre = TRUE), 281500)
})

test_that("OSGB hectare grid reference northing centre", {
  expect_equal(northing(as_gridref("TL211811"), centre = TRUE), 281150)
})

test_that("OSGB 10m grid reference northing centre", {
  expect_equal(northing(as_gridref("TL21118111"), centre = TRUE), 281115)
})

test_that("OSGB 1m grid reference northing centre", {
  expect_equal(northing(as_gridref("TL2111181111"), centre = TRUE), 281111.5)
})

## OSNI lower left corner
test_that("OSNI hectad grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("T28")), 180000)
})

test_that("OSNI tetrad grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("T28A")), 180000)
})

test_that("OSNI monad grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("T2181")), 181000)
})

test_that("OSNI hectare grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("T211811")), 181100)
})

test_that("OSNI 10m grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("T21118111")), 181110)
})

test_that("OSNI 1m grid reference northing lower left corner", {
  expect_equal(northing(as_gridref("T2111181111")), 181111)
})

## OSNI centre
test_that("OSNI hectad grid reference northing centre", {
  expect_equal(northing(as_gridref("T28"), centre = TRUE), 185000)
})

test_that("OSNI tetrad grid reference northing centre", {
  expect_equal(northing(as_gridref("T28A"), centre = TRUE), 181000)
})

test_that("OSNI monad grid reference northing centre", {
  expect_equal(northing(as_gridref("T2181"), centre = TRUE), 181500)
})

test_that("OSNI hectare grid reference northing centre", {
  expect_equal(northing(as_gridref("T211811"), centre = TRUE), 181150)
})

test_that("OSNI 10m grid reference northing centre", {
  expect_equal(northing(as_gridref("T21118111"), centre = TRUE), 181115)
})

test_that("OSNI 1m grid reference northing centre", {
  expect_equal(northing(as_gridref("T2111181111"), centre = TRUE), 181111.5)
})

# hectad
test_that("OSGB hectad", {
  expect_equal(hectad(as_gridref("TL28")), "TL28")
})

test_that("OSGB tetrad", {
  expect_equal(hectad(as_gridref("TL28A")), "TL28")
})

test_that("OSGB monad", {
  expect_equal(hectad(as_gridref("TL2181")),"TL28")
})

test_that("OSGB hectare", {
  expect_equal(hectad(as_gridref("TL211811")), "TL28")
})

test_that("OSGB 10m", {
  expect_equal(hectad(as_gridref("TL21118111")), "TL28")
})

test_that("OSGB 1m", {
  expect_equal(hectad(as_gridref("TL2111181111")), "TL28")
})

test_that("OSNI hectad", {
  expect_equal(hectad(as_gridref("T28")), "T28")
})

test_that("OSNI tetrad", {
  expect_equal(hectad(as_gridref("T28A")), "T28")
})

test_that("OSNI monad", {
  expect_equal(hectad(as_gridref("T2181")),"T28")
})

test_that("OSNI hectare", {
  expect_equal(hectad(as_gridref("T211811")), "T28")
})

test_that("OSNI 10m", {
  expect_equal(hectad(as_gridref("T21118111")), "T28")
})

test_that("OSNI 1m", {
  expect_equal(hectad(as_gridref("T2111181111")), "T28")
})

# pentad
test_that("OSGB hectad", {
  expect_true(is.na(pentad(as_gridref("TL28"))))
})

test_that("OSGB tetrad", {
  expect_true(is.na(pentad(as_gridref("TL28A"))))
})

test_that("OSGB monad", {
  expect_equal(pentad(as_gridref("TL2181")),"TL28SW")
})

test_that("OSGB hectare", {
  expect_equal(pentad(as_gridref("TL211811")), "TL28SW")
})

test_that("OSGB 10m", {
  expect_equal(pentad(as_gridref("TL21118111")), "TL28SW")
})

test_that("OSGB 1m", {
  expect_equal(pentad(as_gridref("TL2111181111")), "TL28SW")
})

test_that("OSNI hectad", {
  expect_true(is.na(pentad(as_gridref("T28"))))
})

test_that("OSNI tetrad", {
  expect_true(is.na(pentad(as_gridref("T28A"))))
})

test_that("OSNI monad", {
  expect_equal(pentad(as_gridref("T2181")),"T28SW")
})

test_that("OSNI hectare", {
  expect_equal(pentad(as_gridref("T211811")), "T28SW")
})

test_that("OSNI 10m", {
  expect_equal(pentad(as_gridref("T21118111")), "T28SW")
})

test_that("OSNI 1m", {
  expect_equal(pentad(as_gridref("T2111181111")), "T28SW")
})

# tetrad
test_that("OSGB hectad", {
  expect_true(is.na(tetrad(as_gridref("TL28"))))
})

test_that("OSGB tetrad", {
  expect_true(is.na(tetrad(as_gridref("TL28A"))))
})

test_that("OSGB monad", {
  expect_equal(tetrad(as_gridref("TL2181")),"TL28A")
})

test_that("OSGB hectare", {
  expect_equal(tetrad(as_gridref("TL211811")), "TL28A")
})

test_that("OSGB 10m", {
  expect_equal(tetrad(as_gridref("TL21118111")), "TL28A")
})

test_that("OSGB 1m", {
  expect_equal(tetrad(as_gridref("TL2111181111")), "TL28A")
})

test_that("OSNI hectad", {
  expect_true(is.na(tetrad(as_gridref("T28"))))
})

test_that("OSNI tetrad", {
  expect_true(is.na(tetrad(as_gridref("T28A"))))
})

test_that("OSNI monad", {
  expect_equal(tetrad(as_gridref("T2181")),"T28A")
})

test_that("OSNI hectare", {
  expect_equal(tetrad(as_gridref("T211811")), "T28A")
})

test_that("OSNI 10m", {
  expect_equal(tetrad(as_gridref("T21118111")), "T28A")
})

test_that("OSNI 1m", {
  expect_equal(tetrad(as_gridref("T2111181111")), "T28A")
})

# monad
test_that("OSGB hectad", {
  expect_true(is.na(monad(as_gridref("TL28"))))
})

test_that("OSGB tetrad", {
  expect_true(is.na(monad(as_gridref("TL28A"))))
})

test_that("OSGB monad", {
  expect_equal(monad(as_gridref("TL2181")),"TL2181")
})

test_that("OSGB hectare", {
  expect_equal(monad(as_gridref("TL211811")), "TL2181")
})

test_that("OSGB 10m", {
  expect_equal(monad(as_gridref("TL21118111")), "TL2181")
})

test_that("OSGB 1m", {
  expect_equal(monad(as_gridref("TL2111181111")), "TL2181")
})

test_that("OSNI hectad", {
  expect_true(is.na(tetrad(as_gridref("T28"))))
})

test_that("OSNI tetrad", {
  expect_true(is.na(tetrad(as_gridref("T28A"))))
})

test_that("OSNI monad", {
  expect_equal(monad(as_gridref("T2181")),"T2181")
})

test_that("OSNI hectare", {
  expect_equal(monad(as_gridref("T211811")), "T2181")
})

test_that("OSNI 10m", {
  expect_equal(monad(as_gridref("T21118111")), "T2181")
})

test_that("OSNI 1m", {
  expect_equal(monad(as_gridref("T2111181111")), "T2181")
})

# hectare
test_that("OSGB hectad", {
  expect_true(is.na(hectare(as_gridref("TL28"))))
})

test_that("OSGB tetrad", {
  expect_true(is.na(hectare(as_gridref("TL28A"))))
})

test_that("OSGB monad", {
  expect_true(is.na(hectare(as_gridref("TL2181"))))
})

test_that("OSGB hectare", {
  expect_equal(hectare(as_gridref("TL211811")), "TL211811")
})

test_that("OSGB 10m", {
  expect_equal(hectare(as_gridref("TL21118111")), "TL211811")
})

test_that("OSGB 1m", {
  expect_equal(hectare(as_gridref("TL2111181111")), "TL211811")
})

test_that("OSNI hectad", {
  expect_true(is.na(hectare(as_gridref("T28"))))
})

test_that("OSNI tetrad", {
  expect_true(is.na(hectare(as_gridref("T28A"))))
})

test_that("OSNI monad", {
  expect_true(is.na(hectare(as_gridref("T2181"))))
})

test_that("OSNI hectare", {
  expect_equal(hectare(as_gridref("T211811")), "T211811")
})

test_that("OSNI 10m", {
  expect_equal(hectare(as_gridref("T21118111")), "T211811")
})

test_that("OSNI 1m", {
  expect_equal(hectare(as_gridref("T2111181111")), "T211811")
})






