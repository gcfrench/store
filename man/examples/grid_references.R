 suppressPackageStartupMessages({
   library(store)
   suppressWarnings({
     library(dplyr)
     library(janitor)
   })
 })

 grid_references <- nbn_demonstration_dataset %>%
   clean_names() %>%
   slice_tail(n = 15) %>%
   select(grid_reference) %>%
   mutate(grid_reference = as_gridref(grid_reference))


