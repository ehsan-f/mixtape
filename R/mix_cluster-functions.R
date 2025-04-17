#' @export
mix_cluster_make <- function(n = 3) {
  #-- Explicitly terminate any existing clusters
  tryCatch(
    stopCluster(v_mix_cl),
    error = function(e) {
      message("No cluster(s) to stop.")
    }
  )

  #-- Make cluster
  v_mix_cl <<- makeCluster(n, type = 'FORK')
  registerDoParallel(v_mix_cl)
  message(paste0(n, ' cluster(s) registered.'))

  #-- Cluster start time
  v_mix_cluster_start_time <<- Sys.time()

}

#' @export
mix_cluster_stop <- function(cl = NULL) {
  #-- Explicitly terminate default cluster
  tryCatch(
    stopCluster(v_mix_cl),
    error = function(e) {
      message("No default cluster(s) to stop.")
    }
  )

  #-- Explicitly terminate specified cluster
  if (!is.null(cl)) {
    tryCatch(
      stopCluster(cl),
      error = function(e) {
        message("No specific cluster(s) to stop.")
      }
    )
  }

  #-- Process info
  tryCatch(
    expr = {
      #- Cluster end time
      v_mix_cluster_end_time <<- Sys.time()

      #- Time taken
      v_time_taken <- difftime(v_mix_cluster_end_time, v_mix_cluster_start_time, units='mins')
      message('Cluster uptime: ', round(as.numeric(v_time_taken), 3), ' mins')
    },
    error = function(e) {
      message("No start time detected.")
    }
  )

}
