#' @export
mix_cluster_make <- function(n = 3) {
  #-- Explicitly terminate any existing clusters
  tryCatch(
    stopCluster(v_mix_cl),
    error = function(e) {
      message("No cluster to stop.")
    }
  )

  #-- Make cluster
  v_mix_cl <<- makeCluster(n, type = 'FORK')
  registerDoParallel(v_mix_cl)
  message(paste0(n, ' cluster(s) registered.'))
}

#' @export
mix_cluster_stop <- function(cl = NULL) {
  #-- Explicitly terminate default cluster
  tryCatch(
    stopCluster(v_mix_cl),
    error = function(e) {
      message("No default cluster to stop")
    }
  )

  #-- Explicitly terminate specified cluster
  if (!is.null(cl)) {
    tryCatch(
      stopCluster(cl),
      error = function(e) {
        message("No specific cluster to stop.")
      }
    )
  }
}
