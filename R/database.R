#' create_locked_keyring
#'
#' @description
#' Add a password protected keyring to the Windows Credential Store.
#' This keyring will be used to store database connection strings.
#'
#' On creating the keyring you will be asked for a password.
#' Remember this password as it will be needed everytime you access
#' the database connection strings stored in this keyring.
#'
#' @seealso More information on securing credentials in R can be found in RStudio's
#' [Securing Credentials](https://db.rstudio.com/best-practices/managing-credentials/)
#' webpage.
#'
#' @family keyring functions
#'
#' @param keyring_name
#'
#' @return character, keyring name
#' @export
#'
#' @examples
#' \dontrun{
#' create_locked_keyring(readline("keyring name: "))
#' }
create_locked_keyring <- function(keyring_name) {
  keyring::keyring_create(keyring_name)
  keyring::keyring_lock(keyring_name)
  keyring::keyring_is_locked(keyring_name)
}

#' get_keyring_status
#'
#' @description
#' Check the current status of the keyring. The default status of the keyring is to be locked,
#' requiring its password to unlock it, to access the stored database connection strings.
#'
#' @param keyring_name character, keyring name
#'
#' @family keyring functions
#'
#' @return message, keyring status
#' @export
#'
#' @examples
#' \dontrun{
#' get_keyring_status(readline("keyring name: "))
#' }
get_keyring_status <- function(keyring_name) {
  if (keyring::keyring_is_locked(keyring_name)) {
    stringr::str_glue("The keyring {keyring_name} is currently locked")
  } else {
    stringr::str_glue("The keyring {keyring_name} is currently unlocked")
  }
}

#' unlock_keyring
#'
#' @description
#' Unlock the keyring requiring keyring password
#'
#' @family keyring functions
#'
#' @param keyring_name character, keyring name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' unlock_keyring(readline("keyring name: "))
#' }
unlock_keyring <- function(keyring_name) {
  keyring::keyring_unlock(keyring_name)
  stringr::str_glue("The keyring {keyring_name} is currently unlocked")
}

#' lock_keyring
#'
#' @description
#' Lock the keyring requiring keyring password
#'
#' @family keyring functions
#'
#' @param keyring_name character, keyring name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' lock_keyring(readline("keyring name: "))
#' }
lock_keyring <- function(keyring_name) {
  keyring::keyring_lock(keyring_name)
  stringr::str_glue("The keyring {keyring_name} is currently locked")
}

#' list_keys
#'
#' @description
#' List the current keys stored on the keyring.
#' Each of these keys store a database connection string.
#'
#' @family keyring functions
#'
#' @param keyring_name character, keyring name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' list_keys(readline("keyring name: "))
#' }
list_keys <- function(keyring_name) {
  keyring::key_list(keyring = keyring_name)
}

#' add_key
#'
#' @description
#' Add a new key to the keyring to store a new database connection string.
#' If the keyring is locked then unlock it first with its password.
#'
#' On creating the key, the name of the key relating to the database is added under the service name
#' and the database connections string is added as a json string in the password dialog box.
#'
#' SQL Server windows authentication: {"server":"SERVER_NAME", "database":"DATABASE_NAME"}
#  PostGresSQL database: {"server":"SERVER_NAME", "database":"DATABASE_NAME", "username":"USERNAME", "password":"PASSWORD"}
#' For SQLEXPRESS on the local machine the server name is "localhost\\SQLEXPRESS"
#'
#' @family keyring functions
#'
#' @param keyring_name, character keyring name
#' @param service_name, character key name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' add_key(readline("keyring name: "), readline("service name: "))
#' }
add_key <- function(keyring_name, service_name){
  if (keyring::keyring_is_locked(keyring_name)) {
    message(stringr::str_glue("The keyring {keyring_name} is currently locked, please unlock with password first"))
    keyring::keyring_unlock(keyring_name)
  }
  message(stringr::str_glue("Supply the service name and database connection details"))
  keyring::key_set(keyring = keyring_name, service = service_name)
  keyring::keyring_lock(keyring_name)
  message(stringr::str_glue("The key {service_name} has been added to the keyring {keyring_name}"))
}

#' delete_key
#'
#' @description
#' Remove a key from the keyring by supplying both the keyring name
#' and name of key to remove
#'
#' @family keyring functions
#'
#' @param keyring_name character, keyring name
#' @param service_name character, key name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' delete_key(readline("keyring name: "), readline("service name: "))
#' }
delete_key <- function(keyring_name, service_name) {
  if (keyring::keyring_is_locked(keyring_name)) {
    message(stringr::str_glue("The keyring {keyring_name} is currently locked, please unlock with password first"))
    keyring::keyring_unlock(keyring_name)
  }
  keyring::key_delete(keyring = keyring_name, service = service_name)
  message(stringr::str_glue("The key {service_name} has been removed from the keyring {keyring_name}"))
  keyring::keyring_lock(keyring_name)
}

#' get_sqlserver_connection
#'
#' @description
#' This function initiates a SQL Server connection using windows authentication,
#' either using stored database credentials through the keyring package or else
#' requesting database credentials in the console. The SQL Server tables are added
#' to the connection pane.
#'
#' @seealso More information on connecting to databases using R can be found in RStudio's
#' [Databases using R](https://db.rstudio.com/) webpages.
#'
#' @family database functions
#'
#' @param keyring_name character, keyring name
#' @param service_name character, name of service storing credentials
#' @param keyring logical, use of keyring TRUE (default) or request credentials (FALSE)
#'
#' @return connection object, SQL Server connection object
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'    library(DBI)
#'    library(odbc)
#'  })
#' })
#' # connect to database, using keyring stored credentials
#' con <- get_sqlserver_connection(keyring_name = "keyring_name",
#'                                 service_name = "database_name",
#'                               keyring = TRUE)
#'
#' # list tables
#' dbListTables(con)
#'
#' # get table
#' database_table <- tbl(con, "table_name") %>% collect()
#'
#' # get query
#' database_query <- dbGetQuery(con, "sql_statement")
#' database_query <- tbl(con, sql("sql_statement")) %>% collect()
#'
#' # disconnect from database
#' dbDisconnect(con)
#' }
get_sqlserver_connection <- function(keyring_name, service_name, keyring = TRUE) {

  # get connection string details
  if (!keyring) {

    # get server details
    server <- readline("SQL server name: ")
    database <- readline("Database name: ")

  } else{

    # get server details
    keyring::keyring_unlock(keyring_name)
    server <- jsonlite::fromJSON(keyring::key_get(keyring = keyring_name, service = service_name))[["server"]]
    database <- jsonlite::fromJSON(keyring::key_get(keyring = keyring_name, service = service_name))[["database"]]
    keyring::keyring_lock(keyring_name)

  }

  # get database connection function
  get_connection <- function(connection_function) {
    tryCatch(connection_function(odbc::odbc(),
                                 driver = "SQL Server",
                                 server = server,
                                 database = database),
             error = function(e) {stop("Connection to SQL SERVER UNSUCCESSFUL", call. = FALSE)})
  }

  # connect to SQL Server database
  con <- get_connection(DBI::dbConnect)

  # add connection to connection pane
  get_connection(connections::connection_open)

  return(con)
}

#' get_mysql_connection
#'
#' @description
#' This function initiates a MySQL connection using stored database credentials
#' through the keyring package. The MySQL tables are added to the connection pane.
#'
#' @seealso More information on connecting to databases using R can be found in RStudio's
#' [Databases using R](https://db.rstudio.com/) webpages.
#'
#' @family database functions
#'
#' @param keyring_name character, keyring name
#' @param service_name character, name of service storing credentials
#'
#' @return connection object, MySQL connection object
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'  library(store)
#'  suppressWarnings({
#'    library(dplyr)
#'    library(DBI)
#'    library(odbc)
#'  })
#' })
#' # connect to database, using keyring stored credentials
#' con <- get_mysql_connection(keyring_name = "keyring_name",
#'                             service_name = "database_name")
#' # list tables
#' dbListTables(con)
#'
#' # get table
#' database_table <- tbl(con, "table_name") %>% collect()
#'
#' # get query
#' database_query <- dbGetQuery(con, "sql_statement")
#' database_query <- tbl(con, sql("sql_statement")) %>% collect()
#'
#' # disconnect from database
#' dbDisconnect(con)
#' }
get_mysql_connection <- function(keyring_name, service_name) {

  # unlock keyring
  keyring::keyring_unlock(keyring_name)

  # get database connection function
  get_connection <- function(connection_function) {
    tryCatch(connection_function(RMariaDB::MariaDB(),
                                 host = jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                                            service = service_name))[["server"]],
                                 dbname = jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                                              service = service_name))[["database"]],
                                 user = jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                                            service = service_name))[["username"]],
                                 password = jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                                                service = service_name))[["password"]]),
             error = function(e) {keyring::keyring_lock(keyring_name)
               stop("Connection to MySQL UNSUCCESSFUL", call. = FALSE)})
  }

  # connect to MySQL database
  con <- get_connection(DBI::dbConnect)

  # add connection to connection pane
  get_connection(connections::connection_open)

  # lock keyring
  keyring::keyring_lock(keyring_name)

  return(con)
}

#' get_postgres_connection
#'
#' @description
#' This function initiates a PostgreSQL connection using stored database credentials
#' through the keyring package. The PostgreSQL tables are added to the connection pane.
#'
#' @seealso More information on connecting to databases using R can be found in RStudio's
#' [Databases using R](https://db.rstudio.com/) webpages.
#'
#' @family database functions
#'
#' @param keyring_name character, keyring name
#' @param service_name name of service storing credentials
#'
#' @return connection object, PostgreSQL connection object
#' @export
#'
#' @examples
#' \dontrun{
#' suppressPackageStartupMessages({
#'   library(store)
#'   suppressWarnings({
#'     library(dplyr)
#'     library(DBI)
#'     library(odbc)
#'   })
#' })
#' # connect to database, using keyring stored credentials
#' con <- get_postgres_connection(keyring_name = "keyring_name",
#'                                service_name = "database_name")
#' # list tables
#' dbListTables(con)
#'
#' # get table
#' database_table <- tbl(con, "table_name") %>% collect()
#'
#' # get query
#' database_query <- dbGetQuery(con, "sql_statement")
#' database_query <- tbl(con, sql("sql_statement")) %>% collect()
#'
#' # import table
#' dbWriteTable(con, Id(schema = "schema_name", table = "table_name"),
#'              value = data_frame, append = TRUE, row.names = FALSE)
#'
#' # disconnect from database
#' dbDisconnect(con)
#' }
get_postgres_connection <- function(keyring_name, service_name) {

  # unlock keyring
  keyring::keyring_unlock(keyring_name)

  # get database connection function
  get_connection <- function(connection_function) {
    postgreSQL <- jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                      service = service_name))[["database"]]
    con <- connection_function(RPostgres::Postgres(),
                               host = jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                                          service = service_name))[["server"]],
                               dbname = postgreSQL,
                               user = jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                                          service = service_name))[["username"]],
                               password = jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                                              service = service_name))[["password"]],
                               port = jsonlite::fromJSON(keyring::key_get(keyring = keyring_name,
                                                                          service = service_name))[["port"]])
  }

  # Connect to PostGreSQL database
  con <- get_connection(DBI::dbConnect)

  # Add connection to connection pane
  get_connection(connections::connection_open)

  # lock keyring
  keyring::keyring_lock(keyring_name)

  return(con)
}

