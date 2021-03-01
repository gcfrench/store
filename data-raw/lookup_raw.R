# country_codes
country_codes <- ISOcodes::ISO_3166_1 %>%
  janitor::clean_names() %>%
  dplyr::rename(country_name = name) %>%
  dplyr::select(alpha_3, country_name)

# add UN regions
un_regions_countries <- vroom::vroom(fs::path(folder_path, "UN regions.csv")) %>%
  janitor::clean_names() %>%
  dplyr::select(region_name, iso_alpha3_code) %>%
  dplyr::rename(alpha_3 = iso_alpha3_code,
                un_region_name = region_name)

country_codes <- country_codes %>%
  dplyr::left_join(un_regions_countries) %>%
  dplyr::mutate(un_region_name = dplyr::case_when(
    alpha_3 == "BES" ~ "Americas",
    alpha_3 == "HKG" ~ "Asia",
    alpha_3 == "MAC" ~ "Asia",
    alpha_3 == "TWN" ~ "Asia",
    TRUE ~ un_region_name)) %>%
  dplyr::bind_rows(c(alpha_3 = "YUG",
                     country_name = "Yugoslavia",
                     un_region_name = "Europe"))

# save in data directory
usethis::use_data(country_codes, overwrite = TRUE)
