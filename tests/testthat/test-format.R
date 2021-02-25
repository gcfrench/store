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

# add_tibble_to_list
test_that("tibble added to list", {
  expect_equal(add_tibble_to_list(tibble::tibble(last_three_letters = c("x", "y", "z")),
                                  .name = "last_three_letters"),
               list(last_three_letters = tibble::tibble(last_three_letters = c("x", "y", "z"))))
})


