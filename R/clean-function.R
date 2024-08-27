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
