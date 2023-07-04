#' @title
#' Create a new keyring
#'
#' @description
#' Add a password protected keyring to the Windows Credential Store.
#' This keyring will be used to store database connection strings.
#'
#' @details
#' On creating the keyring you will be asked for a password.
#' Remember this password as it will be needed everytime you access
#' the database connection strings stored in this keyring.
#'
#' @seealso
#' More information on securing credentials in R can be found in RStudio's
#' [Securing Credentials](https://db.rstudio.com/best-practices/managing-credentials/)
#' webpage.
#'
#' @family keyring functions
#'
#' @param keyring_name A character string providing the name of the keyring.
#'
#' @export
create_locked_keyring <- function(keyring_name) {
  keyring::keyring_create(keyring_name)
  keyring::keyring_lock(keyring_name)
  keyring::keyring_is_locked(keyring_name)
}

#' @title
#' List the keyrings
#'
#' @description
#' List the keyrings present in the Windows Credential Store.
#'
#' @family keyring functions
#'
#' @inheritParams create_locked_keyring
#'
#' @export
list_keyrings <- function(keyring_name) {
  keyring::keyring_list()
}

#' @title
#' Delete a keyring
#'
#' @description
#' Deletes a keyring from the Windows Credential Store.
#'
#' @family keyring functions
#'
#' @inheritParams create_locked_keyring
#'
#' @export
delete_keyring <- function(keyring_name) {
  keyring::keyring_delete(keyring_name)
}

#' @title
#' Get the status of a keyring
#'
#' @description
#' Check the current status of the keyring. The default status of the keyring is to be locked,
#' requiring its password to unlock it, to access the stored database connection strings.
#'
#' @inheritParams create_locked_keyring
#'
#' @family keyring functions
#'
#' @return A message giving the locked status of the keyring.
#'
#' @export
get_keyring_status <- function(keyring_name) {
  if (keyring::keyring_is_locked(keyring_name)) {
    stringr::str_glue("The keyring {keyring_name} is currently locked")
  } else {
    stringr::str_glue("The keyring {keyring_name} is currently unlocked")
  }
}

#' @title
#' Unlock a keyring
#'
#' @description
#' Unlock the keyring requiring keyring password.
#'
#' @family keyring functions
#'
#' @inheritParams create_locked_keyring
#'
#' @export
unlock_keyring <- function(keyring_name) {
  keyring::keyring_unlock(keyring_name)
  stringr::str_glue("The keyring {keyring_name} is currently unlocked")
}

#' @title
#' Lock a keyring
#'
#' @description
#' Lock the keyring requiring keyring password.
#'
#' @family keyring functions
#'
#' @inheritParams create_locked_keyring
#'
#' @export
lock_keyring <- function(keyring_name) {
  keyring::keyring_lock(keyring_name)
  stringr::str_glue("The keyring {keyring_name} is currently locked")
}

#' @title
#' List the keys within a keyring
#'
#' @description
#' List the current keys stored on the keyring.
#' Each of these keys store a database connection string.
#'
#' @family keyring functions
#'
#' @inheritParams create_locked_keyring
#'
#' @return A data frame listing the keys in the keyring.
#'
#' @export
list_keys <- function(keyring_name) {
  keyring::key_list(keyring = keyring_name)
}

#' @title
#' Add a key to a keyring
#'
#' @description
#' Add a new key to the keyring to store a new database connection string.
#' If the keyring is locked then unlock it first with its password.
#'
#' @details
#' On creating the key, the name of the key relating to the database is added under the service name
#' and the database connections string is added as a json string in the password dialog box.
#'
#' * SQL Server windows authentication: \{"server":"SERVER_NAME", "database":"DATABASE_NAME"\}.
#' * PostGresSQL database: \{"server":"SERVER_NAME", "database":"DATABASE_NAME", "username":"USERNAME", "password":"PASSWORD"\}.
#' * For SQLEXPRESS on the local machine the server name is "localhost\\SQLEXPRESS". (need to escape back slash with double back slashes)
#'
#' @family keyring functions
#'
#' @inheritParams create_locked_keyring
#' @param service_name A character string providing the name of the key.
#'
#' @export
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

#' @title
#' View key details
#'
#' @description
#' Returns the JSON string containing the connection string stored in the key.
#' The password to unlock the keyring is required before the key details are returned.
#'
#' @family keyring functions
#'
#' @inheritParams add_key
#'
#' @return The JSON string containing the connection string stored in the key.
#'
#' @export
view_key <- function(keyring_name, service_name) {
  keyring::keyring_lock(keyring_name)
  key_details <- keyring::key_get(keyring = keyring_name, service = service_name) %>%
    stringr::str_remove_all('"')
  keyring::keyring_lock(keyring_name)
  return(key_details)
}

#' @title
#' Delete a key from a keyring
#'
#' @description
#' Remove a key from the keyring by supplying both the keyring name and name of
#' key to remove.
#'
#' @family keyring functions
#'
#' @inheritParams add_key
#'
#' @export
delete_key <- function(keyring_name, service_name) {
  if (keyring::keyring_is_locked(keyring_name)) {
    message(stringr::str_glue("The keyring {keyring_name} is currently locked, please unlock with password first"))
    keyring::keyring_unlock(keyring_name)
  }
  keyring::key_delete(keyring = keyring_name, service = service_name)
  message(stringr::str_glue("The key {service_name} has been removed from the keyring {keyring_name}"))
  keyring::keyring_lock(keyring_name)
}

#' @title
#' Connect to a SQL Server database
#'
#' @description
#' This function initiates a SQL Server connection using windows authentication,
#' either using stored database credentials through the keyring package or else
#' requesting database credentials in the console.
#'
#' @section Connect:
#' ```
#' library(DBI)
#' library(odbc)
#'
#' # connect to database, using keyring stored credentials
#' con <- get_sqlserver_connection(keyring_name = "keyring_name",
#'                                 service_name = "database_name",
#'                                 keyring = TRUE)
#' ```
#'
#' @section Query:
#' More information on working with databases using R can be found in RStudio's
#' [Databases using R](https://db.rstudio.com/) webpages.
#' ```
#' # list tables
#' dbListTables(con)
#'
#' # get table
#' database_table <- tbl(con, "table_name") %>% collect()
#'
#' # get query
#' database_query <- dbGetQuery(con, "sql_statement")
#' database_query <- tbl(con, sql("sql_statement")) %>% collect()
#' ```
#'
#' @section Disconnect:
#' ```
#' dbDisconnect(con)
#' ```
#' @family database functions
#'
#' @inheritParams add_key
#' @param keyring default TRUE uses the keyring, FALSE requests credentials.
#'
#' @return A SQL Server database connection object.
#'
#' @export
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

  return(con)
}

#' @title
#' Connect to a MySQL database
#'
#' @description
#' This function initiates a MySQL connection using stored database credentials
#' through the keyring package.
#'
#' @section Connect:
#' ```
#' library(DBI)
#' library(odbc)
#'
#' # connect to database, using keyring stored credentials
#' con <- get_mysql_connection(keyring_name = "keyring_name",
#'                             service_name = "database_name")
#' ```
#'
#' @inheritSection get_sqlserver_connection Query
#'
#' @inheritSection get_sqlserver_connection Disconnect
#'
#' @family database functions
#'
#' @inheritParams add_key
#'
#' @return A MySQL database connection object.
#'
#' @export
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

  # lock keyring
  keyring::keyring_lock(keyring_name)

  return(con)
}

#' @title
#' Connect to a PostgreSQL database
#'
#' @description
#' This function initiates a PostgreSQL connection using stored database credentials
#' through the keyring package.
#'
#' @section Connect:
#' ```
#' library(DBI)
#' library(odbc)
#'
#' # connect to database, using keyring stored credentials
#' con <- get_postgres_connection(keyring_name = "keyring_name",
#'                                service_name = "database_name")
#' ```
#'
#' @inheritSection get_sqlserver_connection Query
#'
#' @inheritSection get_sqlserver_connection Disconnect
#'
#' @family database functions
#'
#' @inheritParams add_key
#'
#' @return A PostgreSQL database connection object.
#'
#' @export
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

  # lock keyring
  keyring::keyring_lock(keyring_name)

  return(con)
}
