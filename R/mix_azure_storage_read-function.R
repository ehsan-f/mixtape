#' Read data files from Azure Storage
#'
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param prefix Path prefix (folder or full file path) within the container
#' @param storage_key Azure storage account key for authentication
#' @param storage_type Type of storage endpoint ('adls' or 'blob', default: 'adls')
#' @param object_format File format to read ('parquet', 'csv', 'tsv', 'json', or 'rds', default: 'parquet')
#' @param regex_pattern Optional regex to filter file names
#' @param single_file If TRUE, treat prefix as a single file path (default: FALSE)
#' @param n_files Optional maximum number of files to read
#' @param return_list If TRUE, return a list of data frames instead of a single combined data frame (default: FALSE)
#'
#' @return A data frame (or list of data frames if return_list is TRUE)
#'
#' @importFrom AzureStor storage_endpoint list_storage_containers list_storage_files storage_download
#' @importFrom arrow read_parquet
#' @importFrom readr read_csv read_tsv
#' @importFrom jsonlite fromJSON
#' @importFrom purrr list_rbind
#' @export
mix_azure_storage_read <- function(storage_account_name,
                                   container_name,
                                   prefix,
                                   storage_key,
                                   storage_type = 'adls',
                                   object_format = 'parquet',
                                   regex_pattern = NULL,
                                   single_file = F,
                                   n_files = NULL,
                                   return_list = F) {

  #-- Validate inputs
  stopifnot(
    isTRUE(single_file) || isFALSE(single_file),
    isTRUE(return_list) || isFALSE(return_list)
  )

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
  if (single_file == T) {

    v_object_names <- prefix

  } else {

    ds_storage_files <- list_storage_files(v_target_container, prefix, recursive = T)

    v_ext_pattern <- if (object_format == 'json') '\\.json(\\.gz)?$' else paste0('\\.', object_format, '$')
    v_object_names <- ds_storage_files$name |>
      grep(pattern = v_ext_pattern, ignore.case = T, value = T)

    if (!is.null(regex_pattern)) {
      v_object_names <- v_object_names |>
        grep(pattern = regex_pattern, ignore.case = T, value = T)
    }

    if (length(v_object_names) == 0) {
      stop("No ", object_format, " files found at: ", prefix)
    }

    message('Files found: ', length(v_object_names))

    if (!is.null(n_files)) {
      v_object_names <- head(v_object_names, n_files)
      message('Files to read: ', length(v_object_names))
    }

  }

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
      if (grepl('\\.gz$', v_object_names[i], ignore.case = T)) {
        buf <- memDecompress(buf, type = 'gzip')
      }
      fromJSON(rawToChar(buf), simplifyVector = T)
    } else if (object_format == 'rds') {
      con <- gzcon(rawConnection(buf))
      result <- readRDS(con)
      close(con)
      result
    }
  }

  #-- End time
  v_time_taken <- difftime(Sys.time(), v_start_time, units = 'mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  #-- Return data
  if (single_file == T) {
    return(ls_object[[1]])
  } else {
    if (return_list == T) {
      return(ls_object)
    } else {
      ds_object <- ls_object |> list_rbind()
      return(ds_object)
    }
  }


}
