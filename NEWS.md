# store 0.3.0

* Added **eda_variable_collection** to run all the exploratory data analyses on
a data frame which provide examples of using the [dlookr](https://github.com/choonghyunryu/dlookr) and [inspectdf](https://alastairrushworth.github.io/inspectdf/) packages and setting 
the project root directory using [here](https://github.com/r-lib/here) package
* Added **eda_variable_correlation** to display a plot of the correlation matrix
for each variable within a data frame
* Added **eda_variable_outliers** to display the variable outliers within a 
data frame
* Added **eda_variable_distribution** to display the distribution of numerical
variables within a data frame
* Added **eda_variable_summary** to export a summary of the variables 
within a data frame as plots and a table
* Added **add_image_shadow** to add a shadow border to an image, with the example of
parallelization using the [future](https://github.com/HenrikBengtsson/future) and [furrr](https://davisvaughan.github.io/furrr/) packages to batch add border shadows to images within a directory
* Updated **add_new_spreadsheet** added filter parameter to turn off adding filter
to spreadsheet capability
* Updated **country_codes** look up table to include European Union membership
* Added **reduce_image_size** to reduce the size of an image, with the example of
parallelization using the [future](https://github.com/HenrikBengtsson/future) and [furrr](https://davisvaughan.github.io/furrr/) packages to batch reduce images size 
within a directory

# store 0.2.0

* Added **get_postgres_connection** to connect to PostgreSQL database
* Added **display_table** to display data frames in a formatted html table
* Added **county_codes** look up table of ISO-3166 three letter country codes and 
United Nations regions
* Added **add_new_spreadsheet** to add a tibble and optional image to an Excel spreadsheet
* Added **add_tibble_to_list** to prepend multiple tibbles into a list which can
then be converted into a single tibble using dplyr::bind_rows function
* Added **get_mysql_connection** and **get_sqlserver_connection** to connect to 
these databases
* Added functions to use the **keyring** package to store securely store database 
credentials
* Added **extract_comma_delimited_list** and **extract_semicolon_delimited_list** functions.
These are an example of the use of a [function factory](https://adv-r.hadley.nz/function-factories.html),
[tidy evaluation](https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/)
and purrr's [map function](https://purrr.tidyverse.org/reference/map.html) with example using a [nested data frame](https://cran.r-project.org/web/packages/tidyr/vignettes/nest.html)

# store 0.1.0

* Added **Create and update a package** document to information
