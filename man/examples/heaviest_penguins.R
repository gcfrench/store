suppressPackageStartupMessages({
  library(store)
  suppressWarnings({
    library(palmerpenguins)
    library(dplyr)
  })
})

# select top 5 heaviest penguins from each species on each island
heaviest_penguins <- penguins %>%
  select(species, island, body_mass_g) %>%
  group_by(species, island) %>%
  arrange(desc(body_mass_g)) %>%
  slice_head(n = 5) %>%
  ungroup()
heaviest_penguins

