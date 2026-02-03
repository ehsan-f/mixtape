#' Copy a data frame to the clipboard
#'
#' @description
#' Copies a data frame to the system clipboard in a format suitable for pasting into other applications.
#' Works cross-platform on both Unix-like systems and Windows.
#'
#' @param x Data frame to copy
#' @param sep Separator for columns (default: '\\t')
#' @param row.names Whether to include row names (default: FALSE)
#' @param col.names Whether to include column names (default: TRUE)
#' @param ... Additional arguments passed to write.table()
#'
#' @export
copy_table <- function(x, sep = '\t', row.names = FALSE, col.names = TRUE, ...){

  if (.Platform$OS.type == 'unix') {
    write.table(
      x = x,
      file = pipe("pbcopy"),
      sep = sep,
      row.names = row.names,
      col.names = col.names,
      ...
    )
  } else {
    write.table(
      x = x,
      file = 'clipboard',
      sep = sep,
      row.names = row.names,
      col.names = col.names,
      ...
    )
  }

}
