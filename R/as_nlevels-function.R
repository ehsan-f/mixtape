#' @export
as_nlevels = function(x) {
  as.numeric(levels(x))[x]
}
