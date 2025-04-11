#' Convert factor to numeric using level values
#'
#' @description
#' Converts a factor to numeric values based on the numeric interpretation of its levels.
#'
#' @param x Factor to convert
#'
#' @export
as_nlevels = function(x) {
  as.numeric(levels(x))[x]
}
