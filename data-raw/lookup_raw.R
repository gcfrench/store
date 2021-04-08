# country_codes ----------------------------------------------------------------
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

# add 27 European Union member countries
# https://europa.eu/european-union/about-eu/countries_en

european_union <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
        "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
        "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
country_codes <- country_codes %>%
  dplyr::mutate(eu_member = dplyr::case_when(
                                    alpha_3 %in% european_union ~ TRUE,
                                    TRUE ~ FALSE))

# save in data directory
usethis::use_data(country_codes, overwrite = TRUE)

# UK Ireland base map ----------------------------------------------------------
# taken from https://www.datadaptive.com/?pg=14
base_map_path <- choose.files(default = "", caption = "Select base map shape file")
uk_ireland_base_map <- sf::st_read(base_map_path) %>%
  janitor::clean_names() %>%
  dplyr::mutate(uk = as.logical(uk))

# save in data directory
usethis::use_data(uk_ireland_base_map, overwrite = TRUE)

# UK and Ireland 10km grid squares ---------------------------------------------

## Import records
file_path <- choose.files(default = "", caption = "Select dataset")
uk_ireland_tenkm_grid_squares <- readr::read_delim(file_path, delim = "\t")

# save in data directory
usethis::use_data(uk_ireland_tenkm_grid_squares, overwrite = TRUE)
