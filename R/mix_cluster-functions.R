#' @export
mix_cluster_make <- function(n = 3, max_n = 6) {
  tryCatch(
    expr = {
      #-- Packages
      require(future)

      #-- Explicitly terminate any existing clusters
      plan(sequential)

      #-- Make cluster
      n <- min(c(n, max_n))
      plan(multisession, workers = n)
      message(paste0(n, ' cluster(s) registered.'))

      #-- Cluster start time
      v_mix_cluster_start_time <<- Sys.time()
    },
    error = function(e) {
      message(e)
    }
  )

}

#' @export
mix_cluster_stop <- function() {
  #-- Explicitly terminate clusters
  plan(sequential)

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
