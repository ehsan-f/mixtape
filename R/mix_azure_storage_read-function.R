#' Read data from Azure Storage (Blob or ADLS)
#'
#' @description
#' Reads data files from Azure Storage (Blob or ADLS). Supports different file formats
#' including parquet, csv, json, and rds. Handles authentication and error conditions.
#'
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param file_path Path to the files within the container
#' @param regex_pattern Optional regex pattern to filter files (default: NULL)
#' @param single_file Whether file_path points to a single file rather than a directory (default: FALSE)
#' @param storage_type Type of storage ('blob' or 'adls', default: 'blob')
#' @param max_files Maximum number of files to read (optional)
#' @param object_format Format of the files to read ('parquet', 'csv', 'json', 'rds') (default: 'parquet')
#' @param storage_key Azure storage account key for authentication
#' @param var_clean_names Whether to clean variable names (default: FALSE)
#' @param clean_vars Whether to clean variables (convert dates/times) (default: FALSE)
#' @param add_time_fields Whether to add time fields (default: FALSE)
#' @param time_field Field to use for time-based operations if add_time_fields is TRUE
#' @param csv_delim Delimiter for CSV files (default: ',')
#' @param csv_col_names Colnames for CSV files (default: TRUE)
#' @param skip_lines Number of lines to skip (default: 0)
#' @param preserve_partitions Whether to extract and preserve partition columns from directory structure (default: FALSE)
#' @param partition_column Name of the partition column to extract from file paths (required if preserve_partitions is TRUE)
#'
#' @importFrom arrow read_parquet read_json_arrow
#' @importFrom AzureStor storage_endpoint list_storage_containers list_storage_files storage_download
#' @importFrom dplyr mutate across where ends_with
#' @importFrom janitor clean_names
#' @importFrom lubridate as_date as_datetime
#' @importFrom purrr list_rbind
#' @importFrom readr read_delim read_tsv
#' @importFrom tibble as_tibble
#' @export
mix_azure_storage_read <- function(storage_account_name,
                                   container_name,
                                   file_path,
                                   regex_pattern = NULL,
                                   single_file = F,
                                   storage_type = 'adls',
                                   max_files = NULL,
                                   object_format = 'parquet',
                                   storage_key,
                                   var_clean_names = F,
                                   clean_vars = F,
                                   add_time_fields = F,
                                   time_field = NULL,
                                   csv_delim = ',',
                                   csv_col_names = T,
                                   skip_lines = 0,
                                   preserve_partitions = F,
                                   partition_column = NULL) {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('File path: ', file_path)
  message('Storage type: ', storage_type)

  #-- Storage endpoint
  if (tolower(storage_type) == 'adls') {
    v_storage_end_point <- sprintf('https://%s.dfs.core.windows.net', storage_account_name)
  } else {
    v_storage_end_point <- sprintf('https://%s.blob.core.windows.net', storage_account_name)
  }

  #-- Authentication
  v_storage_account <- storage_endpoint(endpoint = v_storage_end_point,
                                        key = storage_key)

  #-- Get files
  ls_storage_containers <- list_storage_containers(v_storage_account)
  v_target_container <- ls_storage_containers[[container_name]]

  if (single_file == T) {
    v_object_names <- file_path
  } else {
    ds_storage_files <- list_storage_files(v_target_container, file_path, recursive = T)

    v_object_names <- ds_storage_files$name |>
      grep(pattern = paste0('\\.', object_format, '$'), ignore.case = T, value = T)

    #-- Regex
    if(!is.null(regex_pattern)) {
      v_object_names <- v_object_names |>
        grep(pattern = regex_pattern, ignore.case = T, value = T)
    }
  }


  #-- RDS Read
  if (tolower(object_format) == 'rds') {

    if (length(v_object_names) == 0) {
      stop("No RDS files found at the specified path")
    }

    if (length(v_object_names) > 1) {
      warning("Multiple RDS files found. Reading only the first file.")
      v_object_names <- v_object_names[1]
    }

    tryCatch(
      expr = {
        #-- Create temp file
        temp_file <- tempfile(fileext = ".rds")

        #-- Download file
        storage_download(v_target_container,
                         src = v_object_names,
                         dest = temp_file)

        #-- Read file
        ls_object <- readRDS(temp_file)

        #-- Remove temp file
        file.remove(temp_file)
        message('Temporary file deleted.')
      },
      error = function(e) {
        #-- Delete temp file
        if (file.exists(temp_file)) {
          file.remove(temp_file)
          message('Temporary file deleted.')
        }
        stop(e)
      }
    )

  } else {
    #-- Read other file types
    ls_object <- NULL
    v_max_files <- length(v_object_names)
    if (!is.null(max_files)) {
      v_max_files <- min(max_files, v_max_files)
    }

    # Check if we should preserve partitions for parquet files
    if (tolower(object_format) == 'parquet' && preserve_partitions && !is.null(partition_column)) {
      message('Preserving partition column: ', partition_column)

      # Extract partition values from file paths
      partition_values <- sapply(v_object_names[1:v_max_files], function(path) {
        # Parse the partition column=value format from the path
        pattern <- paste0(partition_column, "=([^/]+)")
        matches <- regexpr(pattern, path, perl = TRUE)
        if (matches > 0) {
          # Extract just the value part after the =
          extracted <- regmatches(path, matches)
          return(sub(paste0(partition_column, "="), "", extracted))
        } else {
          warning("Could not find partition column in path: ", path)
          return(NA)
        }
      })

      tryCatch(
        expr = {
          for (i in 1:v_max_files) {
            #-- Print progress
            message('Progress: ', i, '/', v_max_files)

            #-- Create temp file
            temp_file <- tempfile(fileext = paste0(".", object_format))

            #-- Download file
            storage_download(v_target_container,
                             src = v_object_names[i],
                             dest = temp_file)

            #-- Read file
            df_temp <- read_parquet(file = temp_file)

            #-- Add partition column if not already present
            if (!partition_column %in% names(df_temp)) {
              df_temp[[partition_column]] <- partition_values[i]
            }

            ls_object[[i]] <- df_temp

            #-- Delete temp file
            file.remove(temp_file)
            message("Temporary file deleted.")
          }
        },
        error = function(e) {
          #-- Delete temp file
          if (exists("temp_file") && file.exists(temp_file)) {
            file.remove(temp_file)
            message("Temporary file deleted.")
          }
          stop(e)
        }
      )
    } else {
      # Original code for other formats or when not preserving partitions
      tryCatch(
        expr = {
          for (i in 1:v_max_files) {
            #-- Print progress
            message('Progress: ', i, '/', v_max_files)

            #-- Create temp file
            temp_file <- tempfile(fileext = paste0(".", object_format))

            #-- Download file
            storage_download(v_target_container,
                             src = v_object_names[i],
                             dest = temp_file)

            #-- Read file
            if (object_format == 'parquet') {
              ls_object[[i]] <- read_parquet(file = temp_file)
            }

            if (object_format == 'csv') {
              ls_object[[i]] <- read_delim(file = temp_file, delim = csv_delim, skip = skip_lines, col_names = csv_col_names)

            }

            if (object_format == 'tsv') {
              ls_object[[i]] <- read_tsv(file = temp_file, skip = skip_lines, col_names = csv_col_names)

            }

            if (object_format == 'json') {
              ls_object[[i]] <- read_json_arrow(file = temp_file) |>
                as_tibble()
            }

            #-- Delete temp file
            file.remove(temp_file)
            message("Temporary file deleted.")
          }
        },
        error = function(e) {
          #-- Delete temp file
          if (exists("temp_file") && file.exists(temp_file)) {
            file.remove(temp_file)
            message("Temporary file deleted.")
          }
          stop(e)
        }
      )
    }

    #-- Reduce list to dataframe
    ls_object <- ls_object |>
      list_rbind()
  }

  #-- Apply transformations only if the object is a data frame/tibble
  if (is.data.frame(ls_object)) {
    #-- Var names to lower
    if (var_clean_names == T) {
      ls_object <- ls_object |>
        clean_names()
    }

    #-- Fix dataset
    if (clean_vars == T) {
      ls_object <- ls_object |>
        mutate(
          across(where(is.character) & ends_with('date'), .fns = as_date),
          across(where(is.character) & ends_with('time'), .fns = as_datetime),
        )
    }

    #-- Add time fields
    if (add_time_fields == T & !is.null(time_field)) {
      ls_object <- ls_object |>
        time_key(x = time_field)
    }
  }

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  #-- Output
  return(ls_object)
}
