#' @export
mix_code_execution <-  function(root_dir = NULL,
                                script_name,
                                google_sheet_id = NULL,
                                sheet_name = 'R_Data_Process_Logs',
                                # gcp_js_path,
                                ...) {

  ##### Packages #####
  require(dplyr)

  ##### Variables #####
  file_location <- paste0(root_dir,
                          script_name,
                          if_else(!grepl('\\.R$', x = script_name), '.R', ''))

  ##### Code Execution #####
  #-- Start Time
  v_start_time <- Sys.time()

  error_message <- NA
  tryCatch(

    {

      source(file = file_location)

    }, error = function(e) return( error_message <<- as.character(conditionMessage(e)) )

  )

  #-- End Time
  v_end_time <- Sys.time()

  #-- Process Info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  ##### Status Report #####
  if (!is.null(google_sheet_id)) {
    current_time <- Sys.time()
    status <- ifelse(!is.na(error_message), 'Failed!', 'Success!')

    ds_error <- tibble(
      script_name = script_name,
      run_time = current_time,
      status = status,
      error = error_message,
      file_location = file_location,
      time_taken_mins = round(as.numeric(v_time_taken), 3)
    )

    #-- Export
    sheet_append(data = ds_error, ss = as_sheets_id(google_sheet_id), sheet = sheet_name)
  }

}
