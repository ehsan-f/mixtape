#' Clean the R environment
#'
#' @description
#' Removes objects from the global environment, closes graphics devices,
#' clears the console, and performs garbage collection.
#' Optionally preserves specified objects.
#'
#' @param except Vector of object names to preserve (optional)
#'
#' @export
clean <- function(except = NULL) {

  ls_env <- ls(envir = .GlobalEnv)

  if (is.null(except)) {
    rm(list = ls_env, envir = .GlobalEnv)
    graphics.off()
    cat("\014")
    gc()
  } else {
    rm(list = ls_env[!(ls_env %in% except)], envir = .GlobalEnv)
    graphics.off()
    cat("\014")
    gc()
  }

}
