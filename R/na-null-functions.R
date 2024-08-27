#' @export
if_na <- function(x, replacement, na = T, value = NULL) {
  if (na == T) {
    x[is.na(x)] <- replacement
  } else if (na == F) {
    x[x %in% value] <- replacement
  }
  x
}

#' @export
if_null <- function(x, replacement = NA) {
  x <- ifelse(is.null(x) | x %in% c("", " ", 'NULL', 'null'), replacement, x)
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
