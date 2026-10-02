#' Read a geo file from Azure Storage into an sf object
#'
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param prefix Full path to the file within the container
#' @param storage_key Azure storage account key for authentication (optional if token is provided or if using managed identity/interactive auth)
#' @param token Azure authentication token object (optional). If supplied along with storage_key, storage_key takes priority. If neither is supplied, attempts to resolve via managed identity or interactive authentication.
#' @param storage_type Type of storage ('blob' or 'adls', default: 'adls')
#' @param driver OGR driver to use ('GeoJSON' or 'GPKG', default: 'GeoJSON')
#'
#' @return An sf object
#'
#' @importFrom AzureStor list_storage_containers storage_download
#' @importFrom sf st_read
#' @export
mix_azure_geo_read <- function(storage_account_name,
                               container_name,
                               prefix,
                               storage_key = NULL,
                               token = NULL,
                               storage_type = 'adls',
                               driver = 'GeoJSON') {

  v_start_time <- Sys.time()
  message('File path: ', prefix)

  v_file_ext <- switch(driver, GeoJSON = 'geojson', GPKG = 'gpkg', tolower(driver))

  if (tolower(storage_type) == 'adls') {
    v_endpoint <- sprintf('https://%s.dfs.core.windows.net', storage_account_name)
  } else {
    v_endpoint <- sprintf('https://%s.blob.core.windows.net', storage_account_name)
  }

  v_storage_account     <- mix_azure_resolve_endpoint(v_endpoint, storage_key, token)
  ls_storage_containers <- list_storage_containers(v_storage_account)
  v_target_container    <- ls_storage_containers[[container_name]]

  temp_file <- tempfile(fileext = paste0('.', v_file_ext))
  on.exit(if (file.exists(temp_file)) file.remove(temp_file), add = TRUE)

  message('Reading: ', prefix)
  storage_download(v_target_container, src = prefix, dest = temp_file)
  df <- sf::st_read(temp_file, quiet = TRUE)

  v_time_taken <- difftime(Sys.time(), v_start_time, units = 'mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  return(df)
}
