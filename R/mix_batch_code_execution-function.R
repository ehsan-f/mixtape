#' @export
mix_batch_code_execution <- function(directory, run_manual_scripts = NA) {
  tryCatch(
    {
      #-- Start time
      v_start_time <- Sys.time()

      #-- Get R scripts
      v_dir <- directory
      v_r_scripts <- dir(v_dir) %>% sort()

      #-- Manual script selection override
      run_manual_scripts <- run_manual_scripts %>% if_null()
      if (!is.na(run_manual_scripts)) {
        run_manual_scripts <- fromJSON(run_manual_scripts)
        run_manual_scripts <- paste0(run_manual_scripts,
                                     if_else(!grepl('\\.R$', x = run_manual_scripts), '.R', ''))

        #- If any manual script does not exist
        if (any(!run_manual_scripts %in% v_r_scripts)) {

          stop(paste0(run_manual_scripts[run_manual_scripts %in% v_r_scripts], ' does not exist', collapse = ' | '),
               call. = F)

        }

        #- Overwrite script names
        v_r_scripts <- run_manual_scripts

      }

      #-- Script paths
      v_r_scripts_path <- paste0(v_dir, '/', v_r_scripts)

      #-- Run code execution
      ds_code_results <- NULL
      for (i in v_r_scripts_path) {
        #- Run script / extract results
        ds_code_results_x <- mix_code_execution(script_path = i)

        #- Bring together
        ds_code_results <- ds_code_results %>%
          bind_rows(ds_code_results_x)
      }

      #-- End time
      v_end_time <- Sys.time()

      #-- Process info
      v_time_taken_mins <- difftime(v_end_time, v_start_time, units = 'mins') %>% as.numeric()

      ds_process_results <- tibble(
        time_taken = v_time_taken_mins,
        start_time = v_start_time,
        end_time = v_end_time
      )

      #-- Final results
      ls_results <- list(
        process_info = ds_process_results,
        code_info = ds_code_results
      )

      #-- Return results
      return(ls_results)

    }, error = function(e) return(as.character(conditionMessage(e)))

  )

}
