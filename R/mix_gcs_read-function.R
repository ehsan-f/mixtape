#' @export
mix_gcs_read <- function(project,
                         bucket,
                         folder_regex = '',
                         object_regex,
                         latest_object_only = F,
                         object_name_wildcard_length = 5,
                         # destination_file_name,
                         object_format = 'parquet',
                         var_clean_names = F,
                         clean_vars = F,
                         add_time_fields = F,
                         time_field = NULL,
                         csv_delim = ',',
                         skip_lines = 0) {

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

  #-- Bucket objects
  ds_bucket_objects <- gcs_list_objects(bucket = bucket) %>%
    filter(grepl(pattern = folder_regex, ignore.case = T, x = name))

  #-- Object names
  v_object_names <- grep(pattern = object_regex, x = ds_bucket_objects$name, ignore.case = T, value = T)

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

  #-- Download objects
  ls_object <- NULL

  tryCatch(

    expr = {
      for (i in 1:length(v_object_names)) {
        #- Print progress
        message('Progress: ', i, '/', length(v_object_names))

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

        if (object_format == 'xlsx') {
          ls_object[[i]] <- read_excel(path = v_file_download_name, skip = skip_lines)
        }

        if (object_format == 'json') {
          ls_object[[i]] <- fromJSON(txt = v_file_download_name) %>%
            as_tibble()
        }

        #-- Remove file
        file.remove(v_file_download_name)
        message('Downloaded file deleted.')

      }
    },

    error = function(e) {
      #-- Remove downloaded file in case of an error
      file.remove(v_file_download_name)
      message('Downloaded file deleted.')

      #-- Output error message
      stop(e)
    }
  )

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
  if (add_time_fields == T) {
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
