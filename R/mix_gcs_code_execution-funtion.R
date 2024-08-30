#' @export
mix_gcs_code_execution <- function(project,
                                   bucket,
                                   folder_regex = '',
                                   object_regex,

                                   #-- mix_code_execution args
                                   google_sheet_id = NULL,
                                   sheet_name = 'R_Code_Logs') {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('Object regex: ', object_regex)

  #-- Packages
  library(dplyr)
  library(googleCloudStorageR)

  #-- Bucket objects
  ds_bucket_objects <- gcs_list_objects(bucket = bucket) %>%
    filter(grepl(pattern = folder_regex, ignore.case = T, x = name))

  #-- Object names
  v_object_names <- grep(pattern = object_regex, x = ds_bucket_objects$name, ignore.case = T, value = T)

  #-- Download objects
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

        #-- Run script
        mix_code_execution(script_path = v_file_download_name,
                           google_sheet_id = google_sheet_id,
                           sheet_name = sheet_name)

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

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

}
