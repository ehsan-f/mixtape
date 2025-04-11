#' Read data from Azure Data Lake Storage
#'
#' @description
#' Reads data files from Azure Data Lake Storage. Supports different file formats
#' including parquet, csv, and json. Handles authentication and error conditions.
#'
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param file_path Path to the files within the container
#' @param max_files Maximum number of files to read (optional)
#' @param object_format Format of the files to read ('parquet', 'csv', 'json') (default: 'parquet')
#' @param storage_sas Shared access signature for Azure authentication
#' @param var_clean_names Whether to clean variable names (default: FALSE)
#' @param clean_vars Whether to clean variables (convert dates/times) (default: FALSE)
#' @param add_time_fields Whether to add time fields (default: FALSE)
#' @param time_field Field to use for time-based operations if add_time_fields is TRUE
#' @param csv_delim Delimiter for CSV files (default: ',')
#' @param skip_lines Number of lines to skip (default: 0)
#'
#' @export
mix_azure_adls_read <- function(storage_account_name,
                                container_name,
                                file_path,
                                max_files = NULL,
                                object_format = 'parquet',
                                storage_sas,
                                var_clean_names = F,
                                clean_vars = F,
                                add_time_fields = F,
                                time_field = NULL,
                                csv_delim = ',',
                                skip_lines = 0) {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('File path: ', file_path)

  #-- Packages
  library(arrow)
  library(dplyr)
  library(janitor)
  library(AzureStor)
  library(purrr)

  #-- Authentication
  v_adls_end_point <- sprintf('https://%s.dfs.core.windows.net', storage_account_name)
  v_adls_storage_account <- storage_endpoint(endpoint = v_adls_end_point,
                                             sas = storage_sas)

  #-- Get files
  ls_adls_containers <- list_storage_containers(v_adls_storage_account)
  v_adls_target_container <- ls_adls_containers[[container_name]]
  ds_adls_files <- list_storage_files(v_adls_target_container, file_path)

  v_object_names <- ds_adls_files$name |>
    grep(pattern = paste0(object_format, '$'), ignore.case = T, value = T)

  #-- Download objects
  ls_object <- NULL
  v_max_files <- length(v_object_names)
  if (!is.null(max_files)) {
    v_max_files <- max_files
  }

  tryCatch(

    expr = {
      for (i in 1:v_max_files) {
        #- Print progress
        message('Progress: ', i, '/', length(v_object_names))

        v_data_path <- sprintf(
          'https://%s.blob.core.windows.net/%s/%s?%s',
          storage_account_name, container_name, v_object_names[i], storage_sas
        )

        #-- Read file
        if (object_format == 'parquet') {
          ls_object[[i]] <- read_parquet(file = v_data_path)
        }

        if (object_format == 'csv') {
          ls_object[[i]] <- read_delim_arrow(file = v_data_path, delim = csv_delim, skip = skip_lines)
        }

        if (object_format == 'json') {
          ls_object[[i]] <- read_json_arrow(file = v_data_path) %>%
            as_tibble()
        }
      }
    },

    error = function(e) {
      #-- Output error message
      stop(e)
    }
  )

  #-- Reduce list to dataframe
  ls_object <- ls_object %>%
    list_rbind()

  #-- Var names to lower
  if (var_clean_names == T) {
    ls_object <- ls_object %>%
      clean_names()
  }

  #-- Fix dataset
  if (clean_vars == T) {
    ls_object <- ls_object %>%
      mutate(
        across(where(is.character) & ends_with('date'), .fns = as_date),
        across(where(is.character) & ends_with('time'), .fns = as_datetime),
      )
  }

  #-- Add time fields
  if (add_time_fields == T & !is.null(time_field)) {
    ls_object <- ls_object %>%
      time_key(x = time_field)
  }

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  #-- Output
  return(ls_object)

}
