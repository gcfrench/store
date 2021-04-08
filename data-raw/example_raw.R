# NBN Gateway Demonstration Dataset

## Import records
file_path <- choose.files(default = "", caption = "Select dataset")
nbn_demonstration_dataset <- readr::read_delim(file_path, delim = "\t") %>%
  dplyr::relocate(SiteKey, .before = SiteName)

# save in data directory
usethis::use_data(nbn_demonstration_dataset, overwrite = TRUE)
