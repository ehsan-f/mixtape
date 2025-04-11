#' Check if an object is a date or datetime
#'
#' @description
#' Checks if an object inherits from Date or POSIXt classes.
#'
#' @param x Object to check
#'
#' @export
is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}
