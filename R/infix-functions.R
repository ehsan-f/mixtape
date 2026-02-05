#' Check if value is within range (inclusive on both ends)
#'
#' @description
#' Tests if x is greater than or equal to `range[1]` AND less than or equal to `range[2]`.
#'
#' @param x Numeric value to test
#' @param range Numeric vector of length 2 defining the range
#'
#' @return Logical vector
#'
#' @export
`%>=<%` <- function(x, range) {
  x >= range[1] & x <= range[2]
}

#' Check if value is within range (inclusive lower, exclusive upper)
#'
#' @description
#' Tests if x is greater than or equal to `range[1]` AND less than `range[2]`.
#'
#' @param x Numeric value to test
#' @param range Numeric vector of length 2 defining the range
#'
#' @return Logical vector
#'
#' @export
`%>= <%` <- function(x, range) {
  x >= range[1] & x < range[2]
}

#' Check if value is within range (exclusive lower, inclusive upper)
#'
#' @description
#' Tests if x is greater than `range[1]` AND less than or equal to `range[2]`.
#'
#' @param x Numeric value to test
#' @param range Numeric vector of length 2 defining the range
#'
#' @return Logical vector
#'
#' @export
`%> =<%` <- function(x, range) {
  x > range[1] & x <= range[2]
}

#' Check if value is within range (exclusive on both ends)
#'
#' @description
#' Tests if x is greater than `range[1]` AND less than `range[2]`.
#'
#' @param x Numeric value to test
#' @param range Numeric vector of length 2 defining the range
#'
#' @return Logical vector
#'
#' @export
`%> <%` <- function(x, range) {
  x > range[1] & x < range[2]
}

#' Limit values to a specified range
#'
#' @description
#' Constrains x to be within the specified range by capping values at `range[1]` (min) and `range[2]` (max).
#'
#' @param x Numeric value to limit
#' @param range Numeric vector of length 2 defining the min and max limits
#'
#' @return Numeric value constrained to the range
#'
#' @export
`%limit%` <- function(x, range) {
  ifelse(x <= range[1], range[1],
         ifelse(x >= range[2], range[2],
                x))
}

#' Find the next highest bracket value
#'
#' @description
#' Returns the smallest value in range that is greater than or equal to x.
#'
#' @param x Numeric value to bracket
#' @param range Numeric vector of bracket values
#'
#' @return The next highest bracket value from range
#'
#' @export
`%bracket_max%` <- function(x, range) {
  range <- sort(range)
  for (i in 1:length(range)) {
    if (x <= range[i]) {
      return(range[i])
    }
  }
}

#' Find the next lowest bracket value
#'
#' @description
#' Returns the largest value in range that is less than or equal to x.
#'
#' @param x Numeric value to bracket
#' @param range Numeric vector of bracket values
#'
#' @return The next lowest bracket value from range
#'
#' @export
`%bracket_min%` <- function(x, range) {
  range <- sort(range, decreasing = T)
  for (i in 1:length(range)) {
    if (x >= range[i]) {
      return(range[i])
    }
  }
}

