# store 0.3.0

* Added **reduce_image_size** to reduce the size of an image, with an example of
parallelization to batch reduce images size within a directory

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
* Added **extract_comma_delimited_list** and **extract_semicolon_delimited_list** functions

# store 0.1.0

* Added **Create and update a package** document to information



