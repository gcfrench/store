suppressPackageStartupMessages({
  library(store)
  suppressWarnings({
    library(palmerpenguins)
    library(dplyr)
    library(stringr)
    library(purrr)
  })
})

penguin_stats <- function(...) {

  # get row
  data <- list(...)

  # get penguins stats
  species <- purrr::pluck(data, "species")
  island <- purrr::pluck(data, "island")
  body_mass_g <- purrr::pluck(data, "body_mass_g")
  sex <- purrr::pluck(data, "sex")
  year <- purrr::pluck(data, "year")

  # print penguin stats
  message(stringr::str_glue("{click()} : The body mass for the {sex} {species} penguin recorded in {year} on {island} island is {body_mass_g} grams"))

}
penguin_stats_slow <- slowly(penguin_stats, rate_delay(0.1))

penguins %>%
  slice_sample(n = 10) %>%
  arrange(desc(body_mass_g)) %>%
  tally_counter(type = "add") %>%
  pwalk(penguin_stats_slow)
