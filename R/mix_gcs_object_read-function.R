#' Read object from Google Cloud Storage
#'
#' @description
#' Reads an R object from Google Cloud Storage as a single file.
#' Supports different file formats and handles error conditions.
#'
#' @param project Google Cloud project ID
#' @param object_name Name of the file to read (without extension)
#' @param bucket Name of the Google Cloud Storage bucket
#' @param prefix prefix path within the bucket (optional)
#' @param object_format Format of the file to read ('parquet', 'csv', 'rds') (default: 'rds')
#'
#' @import arrow
#' @import dplyr
#' @import readr
#' @import googleCloudStorageR
#' @export
mix_gcs_object_read <- function(project,
                                bucket,
                                prefix,
                                object_name,
                                object_format = 'rds') {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('Object: ', object_name)

  #----- Data cleaning
  #-- Ensure trailing slash on prefix
  if (!grepl("/$", prefix)) {
    prefix <- paste0(prefix, "/")
  }

  #----- Single file download and read
  tryCatch(
    expr = {
      #-- File name
      v_file_name <- paste0(object_name, '.', object_format)
      v_gcs_object_path <- paste0(prefix, v_file_name)

      #-- Download from GCS
      message('Downloading from bucket: ', bucket, '/', prefix)

      gcs_get_object(object_name = v_gcs_object_path,
                     bucket = bucket,
                     overwrite = TRUE,
                     saveToDisk = v_file_name)

      #-- Read data
      message('Reading file: ', v_file_name)

      if (tolower(object_format) == 'parquet') {
        result <- read_parquet(file = v_file_name)
      } else if (tolower(object_format) == 'csv') {
        result <- read_csv(file = v_file_name)
      } else if (tolower(object_format) == 'rds') {
        result <- readRDS(file = v_file_name)
      } else {
        stop("Unsupported object_format: ", object_format)
      }

      #-- Remove file
      file.remove(v_file_name)
      message('Downloaded file deleted.')

      #-- End time
      v_end_time <- Sys.time()

      #-- Process info
      v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
      message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

      #-- Return object
      return(result)
    },
    error = function(e) {
      #-- Remove downloaded file in case of an error
      if (exists("v_file_name") && file.exists(v_file_name)) {
        file.remove(v_file_name)
        message('Downloaded file deleted.')
      }

      #-- Output error message
      stop(e)
    }
  )
}
