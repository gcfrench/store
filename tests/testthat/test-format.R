# extract_comma_delimited_list
test_that("comma delimited list extracted", {
  expect_equal(extract_comma_delimited_list(tibble::tibble(last_three_letters = c("x", "y", "z")),
                                            column_name = "last_three_letters"),
               "x, y, z")
})

# extract_semicolon_delimited_list
test_that("semi-colon delimited list extracted", {
  expect_equal(extract_semicolon_delimited_list(tibble::tibble(last_three_letters = c("x", "y", "z")),
                                            column_name = "last_three_letters"),
               "x; y; z")
})
