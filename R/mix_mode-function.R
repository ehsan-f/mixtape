#' Find the mode of a vector
#'
#' @description
#' Finds the most frequently occurring value(s) in a vector.
#' If multiple values occur with the same frequency, all are returned.
#'
#' @param x Vector to find the mode of
#'
#' @export
mix_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
