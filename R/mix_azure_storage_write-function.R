#' Write data to Azure Storage (Blob or ADLS)
#'
#' @description
#' Writes a data frame to Azure Storage (Blob or ADLS). Supports parquet and csv formats,
#' with optional splitting into multiple files by row limit.
#'
#' @param df Data frame to write
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param prefix Path to the destination folder within the container
#' @param storage_key Azure storage account key for authentication
#' @param storage_type Type of storage ('blob' or 'adls', default: 'adls')
#' @param object_format Format for the output files ('parquet', 'csv') (default: 'parquet')
#' @param object_name Base name for the output files (default: 'part_')
#' @param single_file Whether to write as a single file instead of multiple parts (default: FALSE)
#' @param max_rows_per_file Maximum number of rows per file (default: 1000000)
#'
#' @importFrom arrow write_parquet
#' @importFrom AzureStor storage_endpoint list_storage_containers storage_upload
#' @importFrom readr write_csv
#' @export
mix_azure_storage_write <- function(df,
                                    storage_account_name,
                                    container_name,
                                    prefix,
                                    storage_key,
                                    storage_type = 'adls',
                                    object_format = 'parquet',
                                    object_name = 'part_',
                                    single_file = F,
                                    max_rows_per_file = 100000) {

  #-- Start time
  v_start_time <- Sys.time()

  message('Prefix: ', prefix)
  message('Storage type: ', storage_type)

  v_rows <- nrow(df)
  if (v_rows == 0) stop("Cannot write empty data frame")

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

  #-- Folder path trailing slash
  prefix <- paste0(gsub('/$', '', prefix), '/')

  message('Total rows: ', format(v_rows, big.mark = ','))

  #-- Single file upload
  if (single_file == T) {
    v_file_name <- paste0(object_name, '.', object_format)
    temp_file <- tempfile(fileext = paste0('.', object_format))
    on.exit(if (file.exists(temp_file)) file.remove(temp_file), add = TRUE)

    message('Writing: ', v_file_name)

    if (object_format == 'parquet') {
      write_parquet(x = df, sink = temp_file)
    } else if (object_format == 'csv') {
      write_csv(x = df, file = temp_file)
    }

    storage_upload(v_target_container,
                   src = temp_file,
                   dest = paste0(prefix, v_file_name))

  } else {
    #-- Build row index splits
    v_batch_seq <- c(seq(0, v_rows, by = max_rows_per_file), v_rows)
    v_batch_seq <- unique(v_batch_seq)

    v_n_files <- length(v_batch_seq) - 1
    message('Files to write: ', v_n_files)

    for (i in seq_len(v_n_files)) {
      v_file_number <- formatC(i, width = 5, flag = '0')
      v_file_name <- paste0(object_name, v_file_number, '.', object_format)
      temp_file <- tempfile(fileext = paste0('.', object_format))
      on.exit(if (file.exists(temp_file)) file.remove(temp_file), add = TRUE)

      df_batch <- df[(v_batch_seq[i] + 1):v_batch_seq[i + 1], ]

      message('Writing: ', v_file_name, ' (', i, '/', v_n_files, ')')

      if (object_format == 'parquet') {
        write_parquet(x = df_batch, sink = temp_file)
      } else if (object_format == 'csv') {
        write_csv(x = df_batch, file = temp_file)
      }

      storage_upload(v_target_container,
                     src = temp_file,
                     dest = paste0(prefix, v_file_name))
    }
  }

  #-- End time
  v_time_taken <- difftime(Sys.time(), v_start_time, units = 'mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')
  message('Write completed successfully')
}
