#' Read parquet dataset from Google Cloud Storage using Arrow
#'
#' @description
#' Reads parquet files from Google Cloud Storage as a unified dataset using arrow's open_dataset functionality.
#' This is more efficient than reading files one by one as it allows lazy evaluation and partitioned reading.
#'
#' @param bucket Name of the Google Cloud Storage bucket
#' @param prefix Path to required folder containing parquet files
#' @param select Character vector of column names to select (default: NULL for all columns)
#' @param collect Whether to collect the dataset into memory as a tibble (default: TRUE)
#' @param var_clean_names Whether to clean variable names (default: FALSE)
#'
#' @export

mix_gcs_read_parquet_dataset <- function(bucket,
                                         prefix,
                                         select = NULL,
                                         collect = T,
                                         var_clean_names = F) {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Packages
  library(arrow)
  library(dplyr)
  library(janitor)

  #-- Ensure trailing slash on prefix
  if (!grepl("/$", prefix)) {
    prefix <- paste0(prefix, "/")
  }

  #-- Construct GCS URI
  gcs_uri <- paste0('gs://', bucket, '/', prefix)

  message("Opening dataset from: ", gcs_uri)

  #-- Open dataset
  ds_object <- open_dataset(gcs_uri, format = "parquet")

  #-- Select columns if specified
  if (!is.null(select)) {
    message("Selecting columns: ", paste(select, collapse = ", "))
    ds_object <- ds_object |>
      select(all_of(select))
  }

  #-- Collect if requested
  if (collect == T) {
    message("Collecting dataset into memory...")
    ds_object <- ds_object |>
      collect()
  }

  #-- Var names to lower
  if (var_clean_names == T) {
    if (collect == T) {
      ds_object <- ds_object |>
        clean_names()
    } else {
      message("Note: var_clean_names only applies when collect=TRUE")
    }
  }

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  #-- Output
  return(ds_object)

}
