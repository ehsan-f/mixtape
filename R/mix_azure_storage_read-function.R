#' Read data from Azure Storage (Blob or ADLS)
#'
#' @description
#' Reads parquet, csv, tsv, or json files from Azure Storage (Blob or ADLS) directly
#' into memory without writing temp files.
#'
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param prefix Path to the folder or single file within the container
#' @param storage_key Azure storage account key for authentication
#' @param storage_type Type of storage ('blob' or 'adls', default: 'adls')
#' @param object_format Format of the files to read ('parquet', 'csv', 'tsv', 'json') (default: 'parquet')
#' @param regex_pattern Optional regex pattern to filter files (default: NULL)
#'
#' @importFrom arrow read_parquet read_json_arrow
#' @importFrom AzureStor storage_endpoint list_storage_containers list_storage_files storage_download
#' @importFrom purrr list_rbind
#' @importFrom readr read_csv read_tsv
#' @importFrom tibble as_tibble
#' @export
mix_azure_storage_read <- function(storage_account_name,
                                   container_name,
                                   prefix,
                                   storage_key,
                                   storage_type = 'adls',
                                   object_format = 'parquet',
                                   regex_pattern = NULL) {

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
  v_storage_account <- storage_endpoint(endpoint = v_endpoint, key = storage_key)
  ls_storage_containers <- list_storage_containers(v_storage_account)
  v_target_container <- ls_storage_containers[[container_name]]

  #-- List files
  ds_storage_files <- list_storage_files(v_target_container, prefix, recursive = T)

  v_object_names <- ds_storage_files$name |>
    grep(pattern = paste0('\\.', object_format, '$'), ignore.case = T, value = T)

  if (!is.null(regex_pattern)) {
    v_object_names <- v_object_names |>
      grep(pattern = regex_pattern, ignore.case = T, value = T)
  }

  if (length(v_object_names) == 0) {
    stop("No ", object_format, " files found at: ", prefix)
  }

  message('Files found: ', length(v_object_names))

  #-- Read files into memory (no temp files)
  ls_object <- vector("list", length(v_object_names))

  for (i in seq_along(v_object_names)) {
    message('Progress: ', i, '/', length(v_object_names))

    buf <- storage_download(v_target_container, src = v_object_names[i], dest = NULL)

    ls_object[[i]] <- if (object_format == 'parquet') {
      read_parquet(buf)
    } else if (object_format == 'csv') {
      read_csv(I(buf), show_col_types = F)
    } else if (object_format == 'tsv') {
      read_tsv(I(buf), show_col_types = F)
    } else if (object_format == 'json') {
      read_json_arrow(buf) |> as_tibble()
    }
  }

  #-- Combine
  ds_object <- ls_object |> list_rbind()

  #-- End time
  v_time_taken <- difftime(Sys.time(), v_start_time, units = 'mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  return(ds_object)
}
