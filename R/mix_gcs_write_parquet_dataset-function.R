#' Write parquet dataset to Google Cloud Storage using Arrow
#'
#' @description
#' Writes a data frame as a parquet dataset to Google Cloud Storage using arrow's write_dataset functionality.
#' Supports partitioning and automatic file splitting based on minimum number of files.
#'
#' @param df Data frame to write
#' @param bucket Name of the Google Cloud Storage bucket
#' @param prefix Path to destination folder
#' @param partitioning Column names to partition by (default: NULL)
#' @param min_files Minimum number of files to create (default: 100)
#' @param basename_template Template for output file names (default: "part-{i}.parquet")
#' @param compression Compression algorithm ('snappy', 'gzip', 'zstd', 'none') (default: 'snappy')
#'
#' @export
mix_gcs_write_parquet_dataset <- function(df,
                                          bucket,
                                          prefix,
                                          partitioning = NULL,
                                          min_files = 100,
                                          basename_template = "part-{i}.parquet",
                                          compression = 'snappy') {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Packages
  library(arrow)
  library(dplyr)

  #-- Ensure trailing slash on prefix
  if (!grepl("/$", prefix)) {
    prefix <- paste0(prefix, "/")
  }

  #-- Construct GCS URI
  gcs_uri <- paste0('gs://', bucket, '/', prefix)

  message("Writing dataset to: ", gcs_uri)

  if (!is.null(partitioning)) {
    message("Partitioning by: ", paste(partitioning, collapse = ", "))
  }

  #-- Calculate max rows per file with safety checks
  v_rows <- nrow(df)

  if (v_rows == 0) {
    stop("Cannot write empty data frame")
  }

  v_max_rows_per_file <- max(100000, ceiling(v_rows / min_files))

  message("Total rows: ", format(v_rows, big.mark = ","))
  message("Target minimum files: ", min_files)
  message("Max rows per file: ", format(v_max_rows_per_file, big.mark = ","))
  message("Estimated files: ", ceiling(v_rows / v_max_rows_per_file))

  #-- Write dataset
  write_dataset(
    dataset = df,
    path = gcs_uri,
    format = "parquet",
    partitioning = partitioning,
    max_rows_per_file = v_max_rows_per_file,
    basename_template = basename_template,
    compression = compression,
    hive_style = TRUE
  )

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')
  message('Write completed successfully')

}
