#' List files in Azure Storage (Blob or ADLS)
#'
#' @description
#' Lists files in Azure Storage (Blob or ADLS), optionally filtering by file
#' type and/or a regex pattern.
#'
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param prefix Path to the folder within the container
#' @param storage_key Azure storage account key for authentication (optional if token is provided or if using managed identity/interactive auth)
#' @param token Azure authentication token object (optional). If supplied along with storage_key, storage_key takes priority. If neither is supplied, attempts to resolve via managed identity or interactive authentication.
#' @param storage_type Type of storage ('blob' or 'adls', default: 'adls')
#' @param object_format Format to filter files by ('parquet', 'csv', 'tsv', 'json', or NULL for all files) (default: NULL)
#' @param regex_pattern Optional regex pattern to further filter file names (default: NULL)
#'
#' @importFrom AzureStor list_storage_containers list_storage_files
#' @importFrom tibble as_tibble
#' @export
mix_azure_storage_list <- function(
  storage_account_name,
  container_name,
  prefix,
  storage_key = NULL,
  token = NULL,
  storage_type = 'adls',
  object_format = NULL,
  regex_pattern = NULL
) {

  #-- Start time
  v_start_time <- Sys.time()

  message('File path: ', prefix)
  message('Storage type: ', storage_type)

  #-- Storage endpoint
  if (tolower(storage_type) == 'adls') {
    v_endpoint <- sprintf('https://%s.dfs.core.windows.net', storage_account_name)
  } else {
    v_endpoint <- sprintf('https://%s.blob.core.windows.net', storage_account_name)
  }

  #-- Authentication
  v_storage_account <- mix_azure_resolve_endpoint(v_endpoint, storage_key, token)
  ls_storage_containers <- list_storage_containers(v_storage_account)
  v_target_container <- ls_storage_containers[[container_name]]

  #-- List files
  ds_storage_files <- list_storage_files(v_target_container, prefix, recursive = T)

  #-- Filter to files only (exclude directory entries)
  ds_storage_files <- ds_storage_files[!is.na(ds_storage_files$size), ]

  #-- Filter by format
  if (!is.null(object_format)) {
    keep <- grepl(paste0('\\.', object_format, '$'), ds_storage_files$name, ignore.case = T)
    ds_storage_files <- ds_storage_files[keep, ]
  }

  #-- Filter by regex
  if (!is.null(regex_pattern)) {
    keep <- grepl(regex_pattern, ds_storage_files$name, ignore.case = T)
    ds_storage_files <- ds_storage_files[keep, ]
  }

  #-- End time
  v_time_taken <- difftime(Sys.time(), v_start_time, units = 'mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  #-- Return data
  message('Files found: ', nrow(ds_storage_files))

  return(as_tibble(ds_storage_files))
}
