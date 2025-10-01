#' Clean the R environment
#'
#' @description
#' Removes objects from the global environment, closes graphics devices,
#' clears the console, and performs garbage collection.
#' Optionally preserves specified objects and can clean temporary files.
#'
#' @param except Vector of object names to preserve (optional)
#' @param clean_temp Whether to remove temporary files from tempdir() (default: FALSE)
#'
#' @export
clean <- function(except = NULL, clean_temp = F) {

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

  #-- Clean temporary files if requested
  if (clean_temp) {
    temp_dir <- tempdir()
    temp_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

    if (length(temp_files) > 0) {
      n_removed <- sum(file.remove(temp_files))
      message("Removed ", n_removed, " temporary file(s) from ", temp_dir)
    } else {
      message("No temporary files to remove")
    }
  }

}
