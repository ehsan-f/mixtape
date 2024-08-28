#' @export
mix_gcs_data_upload <- function(project,
                                df,
                                object_name,
                                object_name_wildcard_length = 5,
                                bucket,
                                folder = NULL,
                                max_object_size_mb = 50,
                                # destination_file_name,
                                object_format = 'parquet') {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('Object: ', object_name)

  #-- Packages
  library(arrow)
  library(dplyr)
  library(readr)
  library(googleCloudStorageR)

  #----- Data cleaning
  #-- Folder name
  folder <- if_else(is.null(folder), "", paste0(folder, "/"))

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

          #-- Upload to GCS
          message('Uploading to bucket: ', bucket, '/', folder)

          gcs_upload(file = v_file_name,
                     bucket = bucket,
                     type = object_format,
                     name = paste0(folder, v_file_name),
                     predefinedAcl = "default")

          #-- Remove file
          file.remove(v_file_name)
          message('Local file deleted.')

        }
      },

      error = function(e) {
        #-- Remove local file in case of an error
        file.remove(v_file_name)
        message('Local file deleted.')

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

        gcs_upload(file = v_file_name,
                   bucket = bucket,
                   type = object_format,
                   name = paste0(folder, v_file_name),
                   predefinedAcl = "default")

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

  #-- End Time
  v_end_time <- Sys.time()

  #-- Process Info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

}
