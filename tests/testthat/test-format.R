# extract_comma_delimited_list
test_that("comma delimited list extracted", {
  expect_equal(extract_comma_delimited_list(tibble::tibble(last_three_letters = c("x", "y", "z")),
                                            column_name = "last_three_letters"),
               "x, y, z")
})

test_that("comma delimited list excludes NA", {
  expect_equal(extract_comma_delimited_list(tibble::tibble(last_three_letters = c("x", NA_character_)),
                                            column_name = "last_three_letters"),
               "x")
})

test_that("comma delimited list sorts string", {
  expect_equal(extract_comma_delimited_list(tibble::tibble(last_three_letters = c("y", "x")),
                                            column_name = "last_three_letters"),
               "x, y")
})

# extract_semicolon_delimited_list
test_that("semi-colon delimited list extracted", {
  expect_equal(extract_semicolon_delimited_list(tibble::tibble(last_three_letters = c("x", "y", "z")),
                                            column_name = "last_three_letters"),
               "x; y; z")
})

test_that("semi-colon delimited list excludes NA", {
  expect_equal(extract_semicolon_delimited_list(tibble::tibble(last_three_letters = c("x", NA_character_, "y")),
                                            column_name = "last_three_letters"),
               "x; y")
})

test_that("semi-colon delimited list sorts string", {
  expect_equal(extract_semicolon_delimited_list(tibble::tibble(last_three_letters = c("y", "x")),
                                            column_name = "last_three_letters"),
               "x; y")
})

# add_tibble_to_list
test_that("tibble added to list", {
  expect_equal(add_tibble_to_list(tibble::tibble(last_three_letters = c("x", "y", "z")),
                                  .name = "last_three_letters"),
               list(last_three_letters = tibble::tibble(last_three_letters = c("x", "y", "z"))))
})


