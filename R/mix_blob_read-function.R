#' @export
mix_blob_read <- function(storage_account_name,
                          container_name,
                          file_path,
                          max_files = NULL,
                          object_format = 'parquet',
                          blob_sas,
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
                                             sas = blob_sas)

  #-- Get files
  ls_adls_containers <- list_storage_containers(v_adls_storage_account)
  ls_adls_target_container <- ls_adls_containers[[container_name]]
  ds_adls_files <- list_storage_files(ls_adls_target_container, file_path) # |>

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
          storage_account_name, container_name, v_object_names[i], blob_sas
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
