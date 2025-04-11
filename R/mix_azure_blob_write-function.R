#' @export
mix_azure_adls_write <- function(storage_account_name,
                                 container_name,
                                 folder_path,
                                 object_name = 'part_',
                                 df,
                                 object_format = 'parquet',
                                 max_object_size_mb = 50,
                                 object_name_wildcard_length = 5,
                                 blob_sas) {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('Folder path: ', folder_path)

  #-- Packages
  library(arrow)
  library(dplyr)
  library(janitor)
  library(AzureStor)
  library(purrr)

  #-- Authentication
  v_adls_end_point <- sprintf('https://%s.dfs.core.windows.net', storage_account_name)
  v_adls_storage_account <- storage_endpoint(endpoint = v_adls_end_point,
                                             sas = blob_sas)
  ls_adls_containers <- list_storage_containers(v_adls_storage_account)
  v_adls_target_container <- ls_adls_containers[[container_name]]

  #----- Data cleaning
  #-- Folder name
  folder_path <- if_else(is.null(folder_path), "", paste0(folder_path, "/"))

  #-- Actual sizes
  v_size_mb <- format(object.size(df), 'Mb') %>%
    gsub(pattern = '[a-zA-Z]| ', replacement = '') %>%
    as.numeric()

  v_rows <- nrow(df)

  #-- Estimated (conservative) compression factor
  v_compresson_factor <- 4 # 2109 / 468

  #----- Blob Path
  # v_blob_path <- paste0(container_name, '/', folder_path)

  #----- Batch upload
  if (!is.null(max_object_size_mb) & v_rows > 0) {
    #-- Row limit
    v_batch_size_limit <- (v_size_mb / v_compresson_factor) / max_object_size_mb
    v_batch_row_limit <- (v_rows / v_batch_size_limit) %>% round() %>% if_na(replacement = 0)
    v_batch_row_limit <- min(v_batch_row_limit, v_rows)

    #-- Batches
    v_batch_seq <- seq(0, v_rows, by = v_batch_row_limit)
    if (v_batch_seq[length(v_batch_seq)] != v_rows) {
      v_batch_seq <- c(v_batch_seq, v_rows)
    }

    #-- Upload
    tryCatch(

      expr = {
        #-- Wildcard counter
        j <- 0
        v_all_file_names <- NULL

        for (i in 2:length(v_batch_seq)) {
          #-- Wildcard counter
          j <- j + 1

          #-- File name
          v_file_number <- paste0(paste0(rep('0', object_name_wildcard_length), collapse = ''), j) %>%
            substr_right(n = object_name_wildcard_length)
          v_file_name <- paste0(object_name, v_file_number, '.', object_format)

          #-- Write data
          message('Writing to file: ', v_file_name)

          if (object_format == 'parquet') {
            write_parquet(x = df[(v_batch_seq[(i-1)]+1):v_batch_seq[i],],
                          sink = v_file_name)
          }

          if (object_format == 'csv') {
            write_csv(x = df[(v_batch_seq[(i-1)]+1):v_batch_seq[i],],
                      file = v_file_name)
          }

          #-- Combine file names
          v_all_file_names <- c(v_all_file_names, v_file_name)
        }

        #-- Upload data
        message('Uploading to path: ', paste0(container_name, '/', folder_path))

        storage_multiupload(v_adls_target_container,
                            src = v_all_file_names,
                            dest = paste0(folder_path, v_all_file_names))

        #-- Remove file
        file.remove(v_all_file_names)
        message('Local files deleted.')

      },

      error = function(e) {
        #-- Remove local file in case of an error
        file.remove(v_all_file_names)
        message('Local files deleted.')

        #-- Output error message
        stop(e)
      }
    )
  } else {
    #----- Non-batch upload
    #-- Upload
    tryCatch(

      expr = {
        #-- File name
        v_file_name <- paste0(object_name, '.', object_format)

        #-- Write data
        message('Writing to file: ', v_file_name)

        if (object_format == 'parquet') {
          write_parquet(x = df,
                        sink = v_file_name)
        }

        if (object_format == 'csv') {
          write_csv(x = df,
                    file = v_file_name)
        }

        #-- Upload to GCS
        message('Uploading to bucket: ', bucket, '/', folder)

        storage_upload(v_adls_target_container,
                       src = v_file_name,
                       dest = paste0(folder_path, v_file_name))

        #-- Remove file
        file.remove(v_file_name)
        message('Local file deleted.')
      },

      error = function(e) {
        #-- Remove local file in case of an error
        file.remove(v_file_name)
        message('Local file deleted.')

        #-- Output error message
        stop(e)
      }
    )
  }

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

}
