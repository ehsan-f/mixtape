#' Build a one-row nested tibble from a named list of tables
#'
#' @description
#' Packs a named list of tables into a one-row tibble with one list-column per
#' table, converting each table to a tibble first. This is the nested shape
#' used for parquet output (e.g. an API response). Keep tables in a plain list
#' while working with them and only call this when the output is built. Use
#' nested_tbl_extract() to get a table back out.
#'
#' @param ls_tbl A named list of data frames (data.frames, tibbles or data.tables)
#'
#' @return A one-row tibble with one list-column per element of `ls_tbl`
#' @importFrom dplyr as_tibble
#' @export
nested_tbl_build <- function(ls_tbl) {
  as_tibble(lapply(ls_tbl, function(df) list(as_tibble(df))))
}

#' Extract a table from a nested tibble
#'
#' @description
#' Gets one table back out of a one-row nested tibble, such as one built by
#' nested_tbl_build() or read back from a nested parquet file (arrow reads
#' these as length-1 list-columns).
#'
#' @param df A one-row nested tibble
#' @param col Name of the list-column to extract
#'
#' @return The table stored in `col`. If `col` is not a list-column it is
#'   returned as-is, and if `col` does not exist the result is `NULL`.
#' @export
nested_tbl_extract <- function(df, col) {
  x <- df[[col]]
  if (is.list(x)) x <- x[[1]]
  x
}
