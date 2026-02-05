#' Create parallel processing cluster
#'
#' @description
#' Initializes a parallel processing cluster using the future package with a specified
#' number of workers. Automatically terminates any existing clusters before creating a new one.
#'
#' @param n Number of workers to create (default: 3)
#' @param max_n Maximum number of workers allowed (default: 6)
#'
#' @return Creates a multisession cluster and prints a confirmation message
#'
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

#' Stop parallel processing cluster
#'
#' @description
#' Terminates the active parallel processing cluster and reports the total uptime
#' if a cluster was previously created with mix_cluster_make.
#'
#' @return Stops the cluster and prints uptime message
#'
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
