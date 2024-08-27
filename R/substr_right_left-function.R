#' @export
substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

#' @export
substr_left <- function(x, n) {
  substr(x, 1, n)
}
