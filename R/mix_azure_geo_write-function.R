#' Write an sf object to Azure Storage as a geo file
#'
#' @param df sf object to write
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param prefix Path to the destination folder within the container
#' @param storage_key Azure storage account key for authentication
#' @param object_name Base name for the output file (without extension)
#' @param storage_type Type of storage ('blob' or 'adls', default: 'adls')
#' @param driver OGR driver to use ('GeoJSON' or 'GPKG', default: 'GeoJSON')
#'
#' @importFrom AzureStor storage_endpoint list_storage_containers storage_upload
#' @importFrom sf st_write
#' @export
mix_azure_geo_write <- function(df,
                                storage_account_name,
                                container_name,
                                prefix,
                                storage_key,
                                object_name,
                                storage_type = 'adls',
                                driver = 'GeoJSON') {

  v_start_time <- Sys.time()
  message('Prefix: ', prefix)

  v_file_ext <- switch(driver, GeoJSON = 'geojson', GPKG = 'gpkg', tolower(driver))

  if (tolower(storage_type) == 'adls') {
    v_endpoint <- sprintf('https://%s.dfs.core.windows.net', storage_account_name)
  } else {
    v_endpoint <- sprintf('https://%s.blob.core.windows.net', storage_account_name)
  }

  v_storage_account     <- storage_endpoint(endpoint = v_endpoint, key = storage_key)
  ls_storage_containers <- list_storage_containers(v_storage_account)
  v_target_container    <- ls_storage_containers[[container_name]]

  prefix      <- paste0(gsub('/$', '', prefix), '/')
  v_file_name <- paste0(object_name, '.', v_file_ext)
  temp_file   <- tempfile(fileext = paste0('.', v_file_ext))
  on.exit(if (file.exists(temp_file)) file.remove(temp_file), add = TRUE)

  message('Writing: ', v_file_name)
  sf::st_write(df, temp_file, driver = driver, quiet = TRUE)

  storage_upload(v_target_container, src = temp_file, dest = paste0(prefix, v_file_name))

  v_time_taken <- difftime(Sys.time(), v_start_time, units = 'mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')
  message('Write completed successfully')
}
