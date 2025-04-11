#' Extract rightmost characters from a string
#'
#' @description
#' Extracts the rightmost n characters from a string.
#'
#' @param x Character vector
#' @param n Number of characters to extract from the right
#'
#' @export
substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

#' Extract leftmost characters from a string
#'
#' @description
#' Extracts the leftmost n characters from a string.
#'
#' @param x Character vector
#' @param n Number of characters to extract from the left
#'
#' @export
substr_left <- function(x, n) {
  substr(x, 1, n)
}
