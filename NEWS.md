## store 1.1.0

-   Added **REST API endpoints** to the grid reference functions using the [plumber](https://www.rplumber.io/) package.
-   Added **get_cran_package_system_dependencies** to extract a list of system dependencies for CRAN packages listed in a [renv lock file](https://rstudio.github.io/renv/articles/lockfile.html). It is an example of the use of the [httr package](https://httr.r-lib.org/index.html) used to query a REST API, taken from the collation and modification of functions in the David Neuzerling's [getsysres package](https://github.com/mdneuzerling/getsysreqs/tree/main/R), converting the nested list API response to a tibble.
-   Updated **Run validation checks** document to use a [functional sequence](https://riptutorial.com/r/example/5661/functional-sequences) to store verify functions used to validate column names.
-   Added **Git notes** for setting up the Git and GitHub environment, pulling and pushing own and others R projects to Git and GitHub, working on branches and reverting changes, written whilst reading [Happy Git and GitHub for the useR](https://happygitwithr.com/).
-   Added hex logo for package using Colin Fay's [hexmake](https://connect.thinkr.fr/hexmake/) shiny app.

## store 1.0.0 <font size="4">2021-07-02</font>

-   Updated **add_image_shadow** which creates both an internal image and image shadow to include example image taken from [Alison Horst's penguins art](https://allisonhorst.github.io/palmerpenguins/articles/art.html).
-   Updated [roxygen2](https://roxygen2.r-lib.org/index.html) documentation and examples tags for functions and lookups removing repeated text and examples and using [ragg](https://ragg.r-lib.org/) package for saving png images.
-   Added **Docker notes** for writing Dockerfiles, creating Docker images, running Docker containers both locally and using cloud services.
-   Updated **compare_dataset_versions** function to automatically detect the row containing the column names on importing the results file.
-   Updated **country_codes** look up table to link to the table view in documentation. The table view was created using the [gt](https://gt.rstudio.com/) package.
-   Updated **Create and update a package** document.
-   Added **tally_counter** and **click** R6 functions to provide a quick and easy way to monitor progress whilst iterating through a data frame. These functions are written using [R6 object-orientated programming](https://r6.r-lib.org/) with an example of using purrr's [pwalk function](https://purrr.tidyverse.org/reference/map2.html) and ... to pass one row of a data frame at a time into a function.
-   Updated **uk_ireland_base_map** example to include map image using [tmap](https://github.com/mtennekes/tmap) package, following instructions in [Geocomputation with R Chapter 8 Making maps with R](https://geocompr.robinlovelace.net/adv-map.html) by Robin Lovelace, Jakub Nowosad and Jannes Muenchow.
-   Added **create_bounding_box** function to create a bounding box from corner coordinates using the [sf](https://r-spatial.github.io/sf/reference/st.html) package.
-   Added **compare_dataset_versions** function to identify changes between older and newer versions of a dataset, using the [daff](https://github.com/edwindj/daff) package.
-   Added **extract_polygons** function to extract and dissolve polygons from geometry collections using the [sf](https://r-spatial.github.io/sf/) package.
-   Added **PostgreSQL database notes** containing useful SQL, database management and link to Microsoft Access guidance.
-   Updated **extract_delimited_list** parent function to optionally remove duplicated elements in a list.
-   Added **Raster package tutorial notes** made whilst reading [The Raster package](https://rspatial.org/raster/pkg/index.html) by Robert J. Hijmans.

## store 0.4.0 <font size="4">2021-04-23</font>

-   Added **gridsquare_geometry** S3 class of grid reference functions using parent **gridCoords** function from [archived rnbn package](https://github.com/ropensci-archive/rnbn/issues/37) to convert British and Irish grid reference to its square polygon geometry feature. Example uses [dplyr::rowwise](https://dplyr.tidyverse.org/reference/rowwise.html) function to apply function to each grid reference in the dataset.
-   Added **hectad**, **pentad**, **tetrad**, **monad** and **hectare** S3 class of grid reference functions to convert higher resolution grid references using parent **gridRef** function from [archived rnbn package](https://github.com/ropensci-archive/rnbn/issues/37).
-   Added **as_gridref**, **precision**, **projection**, **easting** and **northing** S3 class of grid reference functions using parent **gridCoords** function from [archived rnbn package](https://github.com/ropensci-archive/rnbn/issues/37). S3 classes described in [S3](https://adv-r.hadley.nz/s3.html) chapter of Advanced R.
-   Added **extract_paragraph_delimited_list** family of extract_delimited_list functions. This function adds two carriage returns and new lines as a delimiter.
-   Added **vc_grid_square_intersects** A data frame listing the vice counties intersects with 10km, 2km and 1km grid squares.
-   Added **uk_ireland_tenkm_grid_squares** A data frame listing UK and Ireland 10km grid squares along with their country and whether in the UK, on land or sea.
-   Added **nbn_demonstration_dataset** An example data frame containing bird example records used to demonstrate the NBN Gateway.
-   Updated **get_database_connections** examples to include using [dplyr with databases](https://db.rstudio.com/dplyr/).
-   Added **GDAL commands** document for importing and exporting spatial layers into SQL Server, PostGres and Spatialite.
-   Updated **add_tibble_to_list** to add list to the parent environment calling the function rather than the global environment, as described in the [rlang](https://rlang.r-lib.org/index.html) package.
-   Added **tidy_spatial_data** to help in cleaning spatial data frames using the [sf](https://r-spatial.github.io/sf/) package.
-   Added **Run validation checks** document to information, providing examples of the use of the [assertr](https://cran.r-project.org/web/packages/assertr/vignettes/assertr.html) and [assertthat](https://github.com/hadley/assertthat) packages for validation.
-   Updated **extract_delimited_list** parent function to apply sort, handle NAs and optionally add additional text to separate last two items. Additional **extract_space_delimited_list** function added.
-   Added **uk_ireland_base_map** A simple features data frame of the British and Irish coastline derived from the Oil and Gas Authority's OGA and Lloyd's Register SNS Regional Geological Maps (Open Source) layer.

## store 0.3.0 <font size="4">2021-03-17</font>

-   Added **eda_variable_collection** to run all the exploratory data analyses on a data frame, provides examples of using the [dlookr](https://github.com/choonghyunryu/dlookr) and [inspectdf](https://alastairrushworth.github.io/inspectdf/) packages and setting the project root directory using [here](https://github.com/r-lib/here) package.
-   Added **eda_variable_correlation** to display a plot of the correlation matrix for each variable within a data frame.
-   Added **eda_variable_outliers** to display the variable outliers within a data frame.
-   Added **eda_variable_distribution** to display the distribution of numerical variables within a data frame.
-   Added **eda_variable_summary** to export a summary of the variables within a data frame as plots and a table.
-   Added **add_image_shadow** to add a shadow border to an image, with the example of parallelization using the [future](https://github.com/HenrikBengtsson/future) and [furrr](https://davisvaughan.github.io/furrr/) packages to batch add border shadows to images within a directory.
-   Updated **add_new_spreadsheet** added filter parameter to turn off adding filter to spreadsheet capability.
-   Updated **country_codes** look up table to include European Union membership.
-   Added **reduce_image_size** to reduce the size of an image, with the example of parallelization using the [future](https://github.com/HenrikBengtsson/future) and [furrr](https://davisvaughan.github.io/furrr/) packages to batch reduce images size within a directory.

## store 0.2.0 <font size="4">2021-03-02</font>

-   Added **get_postgres_connection** to connect to PostgreSQL database.
-   Added **display_table** to display data frames in a formatted html table.
-   Added **county_codes** look up table of ISO-3166 three letter country codes and United Nations regions.
-   Added **add_new_spreadsheet** to add a tibble and optional image to an Excel spreadsheet.
-   Added **add_tibble_to_list** to prepend multiple tibbles into a list which can then be converted into a single tibble using dplyr::bind_rows function.
-   Added **get_mysql_connection** and **get_sqlserver_connection** to connect to these databases.
-   Added functions to use the **keyring** package to store securely store database credentials.
-   Added **extract_comma_delimited_list** and **extract_semicolon_delimited_list** functions. These are an example of the use of a [function factory](https://adv-r.hadley.nz/function-factories.html), [tidy evaluation](https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/) and purrr's [map function](https://purrr.tidyverse.org/reference/map.html) with example using a [nested data frame](https://cran.r-project.org/web/packages/tidyr/vignettes/nest.html).

## store 0.1.0 <font size="4">2021-02-20</font>

-   Added **Create and update a package** document to information, providing steps for the creation and documentation of a package using the [usethis](https://usethis.r-lib.org/) and [pkgdown](https://pkgdown.r-lib.org/) packages.
