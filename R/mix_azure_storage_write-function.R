#' Write data to Azure Storage (Blob or ADLS)
#'
#' @description
#' Writes a data frame to Azure Storage (Blob or ADLS) in batches.
#' Supports different file formats and handles error conditions.
#'
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param folder_path Path to the folder within the container
#' @param storage_type Type of storage ('blob' or 'adls', default: 'blob')
#' @param object_name Base name for the output files (default: 'part_')
#' @param df Data frame to write
#' @param object_format Format for the output files ('parquet', 'csv', 'rds') (default: 'parquet')
#' @param max_object_size_mb Maximum size of each output file in MB (default: 50)
#' @param object_name_wildcard_length Length of the wildcard part in object names (default: 5)
#' @param storage_key Azure storage account key for authentication
#'
#' @export
mix_azure_storage_write <- function(storage_account_name,
                                    container_name,
                                    folder_path,
                                    storage_type = 'adls',
                                    object_name = 'part_',
                                    df,
                                    object_format = 'parquet',
                                    max_object_size_mb = 50,
                                    object_name_wildcard_length = 5,
                                    storage_key) {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('Folder path: ', folder_path)
  message('Storage type: ', storage_type)

  #-- Packages
  library(arrow)
  library(dplyr)
  library(janitor)
  library(AzureStor)
  library(purrr)

  #-- Authentication - select appropriate endpoint based on storage type
  if (tolower(storage_type) == 'adls') {
    v_storage_end_point <- sprintf('https://%s.dfs.core.windows.net', storage_account_name)
  } else {
    v_storage_end_point <- sprintf('https://%s.blob.core.windows.net', storage_account_name)
  }

  # Authenticate with storage account key instead of SAS token
  v_storage_account <- storage_endpoint(endpoint = v_storage_end_point,
                                        key = storage_key)
  ls_storage_containers <- list_storage_containers(v_storage_account)
  v_target_container <- ls_storage_containers[[container_name]]

  #----- Data cleaning
  #-- Folder name
  folder_path <- if_else(is.null(folder_path), "", paste0(folder_path, "/"))

  #-- Skip batch upload for RDS files
  if (tolower(object_format) == 'rds') {
    tryCatch(
      expr = {
        #-- File name
        v_file_name <- paste0(object_name, '.', object_format)

        #-- Write data
        message('Writing to file: ', v_file_name)
        saveRDS(object = df, file = v_file_name)

        #-- Upload data
        message('Uploading to container: ', container_name, ', folder path: ', folder_path)

        storage_upload(v_target_container,
                       src = v_file_name,
                       dest = paste0(folder_path, v_file_name))

        #-- Remove file
        file.remove(v_file_name)
        message('Local file deleted.')
      },
      error = function(e) {
        #-- Remove local file in case of an error
        if (file.exists(v_file_name)) {
          file.remove(v_file_name)
          message('Local file deleted.')
        }

        #-- Output error message
        stop(e)
      }
    )
  } else {
    #-- Actual sizes
    v_size_mb <- format(object.size(df), 'Mb') %>%
      gsub(pattern = '[a-zA-Z]| ', replacement = '') %>%
      as.numeric()

    v_rows <- nrow(df)

    #-- Estimated (conservative) compression factor
    v_compresson_factor <- 4 # 2109 / 468

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
            message('Writing to file: ', v_file_name, ' (', j, ' / ', (length(v_batch_seq)-1), ')')

            if (object_format == 'parquet') {
              write_parquet(x = df[(v_batch_seq[(i-1)]+1):v_batch_seq[i],],
                            sink = v_file_name)
            }

            if (object_format == 'csv') {
              write_csv(x = df[(v_batch_seq[(i-1)]+1):v_batch_seq[i],],
                        file = v_file_name)
            }

            #-- Upload data
            message('Uploading to path: ', paste0(container_name, '/', folder_path))

            storage_upload(v_target_container,
                           src = v_file_name,
                           dest = paste0(folder_path, v_file_name))

            #-- Remove file
            file.remove(v_file_name)
            message('Local file deleted.')
          }
        },
        error = function(e) {
          #-- Remove local file in case of an error
          if (file.exists(v_file_name)) {
            file.remove(v_file_name)
            message('Local file deleted.')
          }

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

          #-- Upload data
          message('Uploading to container: ', container_name, ', folder path: ', folder_path)

          storage_upload(v_target_container,
                         src = v_file_name,
                         dest = paste0(folder_path, v_file_name))

          #-- Remove file
          file.remove(v_file_name)
          message('Local file deleted.')
        },
        error = function(e) {
          #-- Remove local file in case of an error
          if (file.exists(v_file_name)) {
            file.remove(v_file_name)
            message('Local file deleted.')
          }

          #-- Output error message
          stop(e)
        }
      )
    }
  }

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')
}
