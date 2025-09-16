#' Read data from Google Cloud Storage
#'
#' @description
#' Reads data files from Google Cloud Storage. Supports different file formats including parquet, csv, xlsx, and json.
#' Can filter objects by regex pattern and optionally process only the latest object.
#'
#' @param project Google Cloud project ID
#' @param bucket Name of the Google Cloud Storage bucket
#' @param prefix Path to required folder
#' @param object_regex Regular expression to filter objects
#' @param latest_object_only Whether to only read the latest object (default: FALSE)
#' @param object_name_wildcard_length Length of the wildcard part in object names (default: 5)
#' @param object_format Format of the objects to read ('parquet', 'csv', 'tsv', 'xlsx', 'json', 'rds') (default: 'parquet')
#' @param var_clean_names Whether to clean variable names (default: FALSE)
#' @param clean_vars Whether to clean variables (convert dates/times) (default: FALSE)
#' @param add_time_fields Whether to add time fields (default: FALSE)
#' @param time_field Field to use for time-based operations if add_time_fields is TRUE
#' @param csv_delim Delimiter for CSV files (default: ',')
#' @param skip_lines Number of lines to skip (default: 0)
#' @param max_files Maximum number of files to read (default: NULL)
#' @param use_direct_url Whether to read parquet files directly from GCS URLs (default: TRUE)
#'
#' @export

mix_gcs_read <- function(project,
                         bucket,
                         prefix,
                         object_regex = '',
                         latest_object_only = F,
                         object_name_wildcard_length = 5,
                         object_format = 'parquet',
                         var_clean_names = F,
                         clean_vars = F,
                         add_time_fields = F,
                         time_field = NULL,
                         csv_delim = ',',
                         skip_lines = 0,
                         max_files = NULL,
                         use_direct_url = T) {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('Object regex: ', object_regex)

  #-- Packages
  library(arrow)
  library(dplyr)
  library(readr)
  library(readxl)
  library(janitor)
  library(googleCloudStorageR)
  library(purrr)

  #-- Ensure trailing slash on prefix
  if (!grepl("/$", prefix)) {
    prefix <- paste0(prefix, "/")
  }

  message("Listing under prefix: ", prefix)

  #-- Bucket objects
  ds_bucket_objects <- gcs_list_objects(bucket = bucket, prefix = prefix) %>%
    #- Object regex
    filter(grepl(pattern = object_regex, ignore.case = T, x = name)) |>
    #- File format
    filter(grepl(pattern = paste0(object_format, '$'), ignore.case = T, x = name))

  #-- Object names
  v_object_names <- ds_bucket_objects$name

  if (latest_object_only == T) {
    ds_object_names <- tibble(
      object_names = v_object_names,
      object_names_modified = gsub(pattern = '\\..*', replacement = '', v_object_names)
    ) %>%
      mutate(
        object_names_wo_wildcard = substr(object_names_modified, 1, nchar(object_names_modified) - object_name_wildcard_length)
      ) %>%
      mutate(
        n_dense_rank = dense_rank(desc(object_names_wo_wildcard))
      ) %>%
      filter(n_dense_rank == 1)

    v_object_names <- ds_object_names$object_names
  }


  #-- Max files to read
  v_max_files <- length(v_object_names)
  if (!is.null(max_files)) {
    v_max_files <- min(max_files, v_max_files)
  }

  #-- Empty list
  ls_object <- NULL

  #-- Direct url reading
  direct_read_success <- FALSE

  if (object_format == 'parquet' & use_direct_url == T) {
    message('Attempting direct URL reading for parquet files')

    direct_read_success <- tryCatch(
      expr = {
        for (i in 1:v_max_files) {
          message('Progress: ', i, '/', v_max_files)

          # Construct GCS URL
          gcs_url <- paste0('gs://', bucket, '/', v_object_names[i])

          # Read directly from GCS
          ls_object[[i]] <- read_parquet(file = gcs_url)
        }
        TRUE  # Return TRUE if all files read successfully
      },
      error = function(e) {
        message('Direct URL reading failed, will fall back to download method: ', e$message)
        FALSE  # Return FALSE if any error occurred
      }
    )
  }

  if (object_format != 'parquet' | !use_direct_url | !direct_read_success) {

    if (object_format == 'parquet' & use_direct_url & !direct_read_success) {
      message('Falling back to download method for parquet files')
      # Reset ls_object in case it has partial data from failed direct read
      ls_object <- NULL
    }

    tryCatch(
      expr = {
        for (i in 1:v_max_files) {
          #- Print progress
          message('Progress: ', i, '/', v_max_files)

          #-- Set file download name
          v_file_download_name <- v_object_names[i]
          v_file_download_name <- substr(x = v_file_download_name,
                                         start = (gregexpr(pattern = '/', text = v_file_download_name, ignore.case = T) %>% unlist() %>% max()) + 1,
                                         stop = nchar(v_file_download_name))
          v_file_download_name <- paste0('gcs_', v_file_download_name)

          #-- Download
          gcs_get_object(object_name = v_object_names[i],
                         bucket = bucket,
                         overwrite = T,
                         saveToDisk = v_file_download_name)

          #-- Read file
          if (object_format == 'parquet') {
            ls_object[[i]] <- read_parquet(file = v_file_download_name)
          }

          if (object_format == 'csv') {
            ls_object[[i]] <- read_delim(file = v_file_download_name, delim = csv_delim, skip = skip_lines)
          }

          if (object_format == 'tsv') {
            ls_object[[i]] <- read_tsv(file = v_file_download_name, skip = skip_lines)
          }

          if (object_format == 'xlsx') {
            ls_object[[i]] <- read_excel(path = v_file_download_name, skip = skip_lines)
          }

          if (object_format == 'json') {
            ls_object[[i]] <- read_json_arrow(file = v_file_download_name) %>%
              as_tibble()
          }

          if (object_format == 'rds') {
            ls_object[[i]] <- readRDS(file = v_file_download_name) %>%
              as_tibble()
          }

          #-- Remove file
          file.remove(v_file_download_name)
          message('Downloaded file deleted.')
        }
      },

      error = function(e) {
        #-- Remove downloaded file in case of an error
        if (exists("v_file_download_name") && file.exists(v_file_download_name)) {
          file.remove(v_file_download_name)
          message('Downloaded file deleted.')
        }

        #-- Output error message
        stop(e)
      }
    )
  }

  #-- Reduce list to dataframe
  ds_object <- ls_object %>%
    list_rbind()

  #-- Var names to lower
  if (var_clean_names == T) {
    ds_object <- ds_object %>%
      clean_names()
  }

  #-- Fix dataset
  if (clean_vars == T) {
    ds_object <- ds_object %>%
      mutate(
        across(where(is.character) & ends_with('date'), .fns = as_date),
        across(where(is.character) & ends_with('time'), .fns = as_datetime),
      )
  }

  #-- Add time fields
  if (add_time_fields == T & !is.null(time_field)) {
    ds_object <- ds_object %>%
      time_key(x = time_field)
  }

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  #-- Output
  return(ds_object)

}
