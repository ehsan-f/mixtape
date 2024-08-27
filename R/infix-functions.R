#' @export
`%>=<%` <- function(x, range) {
  x >= range[1] & x <= range[2]
}

#' @export
`%>= <%` <- function(x, range) {
  x >= range[1] & x < range[2]
}

#' @export
`%> =<%` <- function(x, range) {
  x > range[1] & x <= range[2]
}

#' @export
`%> <%` <- function(x, range) {
  x > range[1] & x < range[2]
}

#' @export
`%limit%` <- function(x, range) {
  ifelse(x <= range[1], range[1],
         ifelse(x >= range[2], range[2],
                x))
}

#' @export
`%bracket_max%` <- function(x, range) {
  range <- sort(range)
  for (i in 1:length(range)) {
    if (x <= range[i]) {
      return(range[i])
    }
  }
}

#' @export
`%bracket_min%` <- function(x, range) {
  range <- sort(range, decreasing = T)
  for (i in 1:length(range)) {
    if (x >= range[i]) {
      return(range[i])
    }
  }
}

