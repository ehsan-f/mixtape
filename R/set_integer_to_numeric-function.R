#' Cast integer columns to numeric
#'
#' @description
#' Keeps R's output types aligned with the PySpark side, where the equivalent
#' final cast also widens "integer"/"long" columns to double (see
#' set_integer_to_numeric() in the .py config files) - integer64/bigint
#' columns otherwise round-trip awkwardly between R and Spark.
#'
#' @param df A data frame
#'
#' @return The input data frame with integer columns cast to numeric (Dates
#'   left as-is)
#' @export
set_integer_to_numeric <- function(df) {
  #- Skip integer-backed Dates (e.g. seq() on Dates in R >= 4.5, data.table
  #  IDate) - as.numeric() would silently drop their class
  df[] <- lapply(df, function(x) if (is.integer(x) && !inherits(x, c("Date", "IDate"))) as.numeric(x) else x)
  df
}
