#' Upload object to Google Cloud Storage
#'
#' @description
#' Uploads an R object to Google Cloud Storage as a single file.
#' Supports different file formats and handles error conditions.
#'
#' @param project Google Cloud project ID
#' @param object R object to upload
#' @param object_name Name for the output file (without extension)
#' @param bucket Name of the Google Cloud Storage bucket
#' @param prefix prefix path within the bucket (optional)
#' @param object_format Format for the output file ('parquet', 'csv', 'rds') (default: 'rds')
#'
#' @export
mix_gcs_object_upload <- function(project,
                                  bucket,
                                  prefix,
                                  object,
                                  object_name,
                                  object_format = 'rds') {

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
  #-- Ensure trailing slash on prefix
  if (!grepl("/$", prefix)) {
    prefix <- paste0(prefix, "/")
  }

  #----- Single file upload
  tryCatch(
    expr = {
      #-- File name
      v_file_name <- paste0(object_name, '.', object_format)

      #-- Write data
      message('Writing to file: ', v_file_name)

      if (tolower(object_format) == 'parquet') {
        write_parquet(x = object, sink = v_file_name)
      }

      if (tolower(object_format) == 'csv') {
        write_csv(x = object, file = v_file_name)
      }

      if (tolower(object_format) == 'rds') {
        saveRDS(object = object, file = v_file_name)
      }

      #-- Upload to GCS
      message('Uploading to bucket: ', bucket, '/', prefix)

      gcs_upload(file = v_file_name,
                 bucket = bucket,
                 type = object_format,
                 name = paste0(prefix, v_file_name),
                 predefinedAcl = "default")

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

  #-- End Time
  v_end_time <- Sys.time()

  #-- Process Info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

}
