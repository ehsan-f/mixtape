#' Replace NA values in a vector
#'
#' @description
#' Replaces NA values or specific values in a vector with a replacement value.
#'
#' @param x Vector to process
#' @param replacement Value to use as replacement
#' @param na Whether to replace NA values (TRUE) or specific values (FALSE) (default: TRUE)
#' @param value Specific values to replace when na=FALSE
#'
#' @export
if_na <- function(x, replacement, na = T, value = NULL) {
  if (na == T) {
    x[is.na(x)] <- replacement
  } else if (na == F) {
    x[x %in% value] <- replacement
  }
  x
}

#' Replace NULL, infinite, or empty values in a vector
#'
#' @description
#' Replaces NULL, infinite, empty strings, or special text values with a replacement value.
#'
#' @param x Vector to process
#' @param replacement Value to use as replacement (default: NA)
#'
#' @export
if_null <- function(x, replacement = NA) {
  x <- ifelse(is.null(x) | is.infinite(x) | x %in% c("", " ", 'NULL', 'null'), replacement, x)
  if (length(x) == 0) {
    x <- replacement
  }
  x
}


# null.if.cb <- function(x, replacement = NA) {
#   x <- ifelse(is.null(x) | x %in% c("", " ", "NULL", 'null', '{ND}', 'ND', '{OB}', 'OB', -999997, -999999, '-999999', '-999998', '-999997', '-999996'), replacement, x)
#   if (length(x) == 0) {
#     x <- replacement
#   }
#   x
# }
