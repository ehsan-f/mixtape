#' Write dataset to Google Cloud Storage using Arrow
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
#' @param object_format Format of the file to read ('parquet', 'csv') (default: 'parquet')
#' @param basename_template Template for output file names (default: "part-{i}.parquet")
#'
#' @import arrow
#' @import dplyr
#' @export
mix_gcs_write_arrow_dataset <- function(df,
                                        bucket,
                                        prefix,
                                        partitioning = NULL,
                                        min_files = 100,
                                        object_format = 'parquet',
                                        single_file = F,
                                        basename_template = "part-{i}") {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Construct GCS URI
  gcs_uri <- paste0('gs://', bucket, '/', prefix)

  #-- Partitioning
  if (!is.null(partitioning)) {
    message("Partitioning by: ", paste(partitioning, collapse = ", "))
  }

  #-- Calculate max rows per file with safety checks
  v_rows <- nrow(df)

  if (v_rows == 0) {
    stop("Cannot write empty data frame")
  }

  #----- Write dataset
  if (object_format == 'csv') {

    #-- Ensure trailing file format on gcs_Uri
    if (!grepl(".csv$", gcs_uri)) {
      gcs_uri <- paste0(gcs_uri, ".csv")
    }

    #-- Write dataset
    message("Writing CSV dataset to: ", gcs_uri)

    write_csv_arrow(
      x = df,
      sink = gcs_uri,
      include_header = TRUE
    )

  } else if (object_format == 'parquet' & single_file == T) {

    #-- Ensure trailing file format on gcs_Uri
    if (!grepl(".parquet$", gcs_uri)) {
      gcs_uri <- paste0(gcs_uri, ".parquet")
    }

    #-- Write dataset
    message("Writing PARQUET dataset to: ", gcs_uri)

    write_parquet(
      x = df,
      sink = gcs_uri
    )

  } else {

    #-- Ensure trailing slash on gcs_uri
    if (!grepl("/$", gcs_uri)) {
      gcs_uri <- paste0(gcs_uri, "/")
    }

    #-- File upload info
    v_max_rows_per_file <- max(100000, ceiling(v_rows / min_files))

    message("Total rows: ", format(v_rows, big.mark = ","))
    message("Target minimum files: ", min_files)
    message("Max rows per file: ", format(v_max_rows_per_file, big.mark = ","))
    message("Estimated files: ", ceiling(v_rows / v_max_rows_per_file))

    #-- Write dataset
    message("Writing PARQUET dataset to: ", gcs_uri)

    write_dataset(
      dataset = df,
      path = gcs_uri,
      format = object_format,
      partitioning = partitioning,
      max_rows_per_file = v_max_rows_per_file,
      basename_template = paste0(basename_template, '.', object_format),
      hive_style = TRUE
    )
  }

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')
  message('Write completed successfully')

}
