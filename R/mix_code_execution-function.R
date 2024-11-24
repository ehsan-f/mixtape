#' @export
mix_code_execution <-  function(script_path = NULL,
                                script_url = NULL,
                                google_sheet_id = NULL,
                                sheet_name = 'R_Code_Logs',
                                script_prefix = NULL,
                                ...) {

  ##### Packages #####
  library(dplyr)
  library(devtools)

  ##### Variables #####
  script_path <- paste0(script_path,
                        if_else(!grepl('\\.R$', x = script_path), '.R', ''))

  #-- Script name (output)
  script_name <- gsub(pattern = '.*/', replacement = '', x = script_path)

  #-- Add prefix if provided (output)
  if (!is.null(script_prefix)) {
    script_name <- paste0(script_prefix, script_name)
  }

  ##### Code Execution #####
  #-- Start Time
  v_start_time <- Sys.time()

  error_message <- NA
  tryCatch(
    {
      if (!is.null(script_url)) {
        source_url(url = script_url)
      } else {
        source(file = script_path)
      }
    }, error = function(e) return( error_message <<- as.character(conditionMessage(e)) )
  )

  #-- End Time
  v_end_time <- Sys.time()

  #-- Process Info
  v_status <- ifelse(!is.na(error_message), 'Failed!', 'Success!')
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')
  message('Status: ', v_status)

  ##### Status Report #####
  v_current_time <- Sys.time()

  ds_status <- tibble(
    script_name = script_name,
    run_time = v_current_time,
    status = v_status,
    error = error_message,
    time_taken_mins = round(as.numeric(v_time_taken), 3)
  )

  #-- Google Sheet output
  if (!is.null(google_sheet_id)) {
    #-- Export
    sheet_append(data = ds_status, ss = as_sheets_id(google_sheet_id), sheet = sheet_name)
  }

  #-- Return results
  return(invisible(ds_status))

}
