#' @export
mix_gdr_read <- function(file_id, read_fn) {

  #-- Start time
  v_start_time <- Sys.time()

  #-- Packages
  library(dplyr)
  library(googledrive)

  #-- Google Drive id
  v_drive_file_id <- as_id(file_id)

  tryCatch(

    expr = {

      #-- Download File
      ds_download <- drive_download(file = v_drive_file_id, overwrite = T)

      #-- Read file
      ds_object <- read_fn(ds_download$local_path)

      #-- Remove file
      file.remove(ds_download$local_path)
      message('Downloaded file deleted.')

    },

    error = function(e) {
      #-- Remove downloaded file in case of an error
      file.remove(ds_download$local_path)
      message('Downloaded file deleted.')

      #-- Output error message
      stop(e)
    }
  )

  #-- Delete downloaded objects
  rm(ds_download)

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  #-- Output
  return(ds_object)

}
