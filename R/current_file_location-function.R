#' Get the directory of the current script
#'
#' @description
#' Determines the directory path of the currently executing R script.
#' Works in both RStudio and when running from command line.
#'
#' @export
current_file_location <-  function() {
  #-- TEst

  this_file <- commandArgs() |>
    tibble::enframe(name = NULL) |>
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') |>
    dplyr::filter(key == "--file") |>
    dplyr::pull(value)

  if (length(this_file)==0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }

  return(dirname(this_file))
}
