#' Format numeric values with thousand separators
#'
#' @description
#' Formats numeric values with thousand separators and specified decimal places.
#'
#' @param x Numeric vector to format
#' @param p Number of decimal places (default: 1)
#'
#' @export
pretty_num <- function(x, p = 1) {
  x <- ifelse(is.na(x), NA, prettyNum(round(x, p), big.mark = ","))
  x
}

#' Format numeric values as currency
#'
#' @description
#' Formats numeric values as currency with thousand separators and specified decimal places.
#'
#' @param x Numeric vector to format
#' @param p Number of decimal places (default: 0)
#' @param curr Currency symbol (default: "")
#'
#' @export
pretty_curr <- function(x, p = 0, curr = "") {
  x <- ifelse(is.na(x), NA, paste(curr, prettyNum(round(x, p), big.mark = ","), sep = " "))
  x
}

#' Format numeric values as percentages
#'
#' @description
#' Formats numeric values as percentages with specified decimal places.
#'
#' @param x Numeric vector to format (in decimal form, e.g., 0.75 for 75%)
#' @param p Number of decimal places (default: 2)
#'
#' @export
pretty_perc <- function(x, p = 2) {
  x <- ifelse(is.na(x), NA, paste(round(x*100, p), '%', sep = " "))
  x
}
