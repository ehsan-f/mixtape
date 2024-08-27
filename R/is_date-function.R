#' @export
is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}
