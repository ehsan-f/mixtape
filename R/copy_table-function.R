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
