#' Read dataset from Google Cloud Storage using Arrow
#'
#' @description
#' Reads parquet files from Google Cloud Storage as a unified dataset using arrow's open_dataset functionality.
#' This is more efficient than reading files one by one as it allows lazy evaluation and partitioned reading.
#'
#' @param bucket Name of the Google Cloud Storage bucket
#' @param prefix Path to required folder containing parquet files
#' @param select Character vector of column names to select (default: NULL for all columns)
#' @param select_regex Regular expression to select columns matching pattern (default: NULL)
#' @param object_format Format of the file to read ('parquet', 'csv', 'rds') (default: 'parquet')
#' @param collect Whether to collect the dataset into memory as a tibble (default: TRUE)
#' @param var_clean_names Whether to clean variable names (default: FALSE)
#'
#' @importFrom arrow open_csv_dataset open_dataset
#' @importFrom dplyr select any_of collect
#' @importFrom janitor clean_names
#' @export

mix_gcs_read_arrow_dataset <- function(bucket,
                                       prefix,
                                       select = NULL,
                                       select_regex = NULL,
                                       object_format = 'parquet',
                                       collect = T,
                                       var_clean_names = F) {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Construct GCS URI
  gcs_uri <- paste0('gs://', bucket, '/', prefix)

  message("Opening dataset from: ", gcs_uri)

  #-- Open dataset
  if (object_format == 'csv') {
    ds_object <- open_csv_dataset(gcs_uri)
  } else {
    ds_object <- open_dataset(gcs_uri, format = object_format)
  }

  v_object_vars <- names(ds_object)

  #-- Select columns if specified
  if (!is.null(select) | !is.null(select_regex)) {
    v_selected_vars <- c()

    if (!is.null(select)) {
      v_selected_vars <- select

      #-- Check for missing columns
      v_missing_vars <- setdiff(select, v_object_vars)
      if (length(v_missing_vars) > 0) {
        message("Warning: The following variables are not available in the dataset: ",
                paste(v_missing_vars, collapse = ", "))
      }
    }

    if (!is.null(select_regex)) {
      v_selected_vars_regex <- grep(pattern = select_regex, x = v_object_vars, ignore.case = T, value = T)
      v_selected_vars <- c(v_selected_vars, v_selected_vars_regex) |> unique()
    }

    message("Selecting columns: ", paste(head(v_selected_vars, 20), collapse = ", "),
            if(length(v_selected_vars) > 20) " ..." else "")

    ds_object <- ds_object |>
      select(any_of(v_selected_vars))
  }

  #-- Collect if requested
  if (collect == T) {
    message("Collecting dataset into memory...")
    ds_object <- ds_object |>
      collect()

    #-- Var names to lower
    if (var_clean_names == T) {
      ds_object <- ds_object |>
        clean_names()
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
