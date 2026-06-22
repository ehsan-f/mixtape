#' Read data from a Databricks SQL Warehouse over ODBC
#'
#' Opens a DBI connection to the SQL Warehouse defined in ls_config$databricks
#' (authenticating with a Personal Access Token), runs a query (or reads a whole
#' table), pulls the result into a data frame, and always disconnects on exit.
#' A simple connect -> read -> disconnect helper, mirroring mix_azure_storage_read.
#'
#' Requires: the Databricks ODBC (Simba Spark) driver installed at driver_path,
#' the `odbc` R package, and DATABRICKS_HOST / DATABRICKS_HTTP_PATH /
#' DATABRICKS_PAT set in .Renviron.
#'
#' Provide exactly one of `query` or `table`:
#'   - query: any SQL string, e.g. "SELECT * FROM cat.schema.tbl WHERE x > 1"
#'   - table: a fully-qualified table name, e.g. "serene_data_science.04_model_layer.data_dictionary_final"
#'
#' @param query Optional SQL query string to execute
#' @param table Optional fully-qualified table name to read in full
#' @param config_db Databricks config list (ls_config$databricks)
#'
#' @return A tibble with the query result
#'
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom odbc databricks
#' @importFrom tibble as_tibble
#' @export
mix_databricks_read <- function(query = NULL,
                                table = NULL,
                                config_db = ls_config$databricks) {

  #-- Validate inputs (need exactly one of query / table)
  stopifnot(xor(is.null(query), is.null(table)))

  if (is.null(query)) {
    query <- sprintf('SELECT * FROM %s', table)
  }

  #-- Credentials / connection details
  path <- config_db$http_path
  driver <- config_db$driver_path
  token <- Sys.getenv(config_db$token_env_var)

  if (!nzchar(config_db$host) || !nzchar(path)) {
    stop("Databricks host / http_path not set. Add DATABRICKS_HOST and ",
         "DATABRICKS_HTTP_PATH to .Renviron and restart R.")
  }
  if (!nzchar(token)) {
    stop("Databricks token not found in env var '", config_db$token_env_var,
         "'. Add it to .Renviron and restart R.")
  }

  #-- Start time
  v_start_time <- Sys.time()
  message('Querying Databricks: ', query)

  #-- Connect. odbc::databricks() supplies the SSL/Thrift/AuthMech/UID defaults
  #-- and reads DATABRICKS_HOST + DATABRICKS_TOKEN from the environment. We only
  #-- override the driver path (not registered in /etc/odbcinst.ini) and the
  #-- warehouse path. Guarantee disconnect even on error.
  Sys.setenv(DATABRICKS_TOKEN = token)

  con <- dbConnect(
    databricks(),
    driver   = driver,
    httpPath = path
  )
  on.exit(dbDisconnect(con), add = TRUE)

  #-- Read
  ds_object <- dbGetQuery(con, query) |> as_tibble()

  #-- End time
  v_time_taken <- difftime(Sys.time(), v_start_time, units = 'mins')
  message('Rows read: ', nrow(ds_object),
          ' | Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  return(ds_object)
}
