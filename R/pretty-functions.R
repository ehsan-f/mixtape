#' @export
pretty_num <- function(x, p = 1) {
  x <- ifelse(is.na(x), NA, prettyNum(round(x, p), big.mark = ","))
  x
}

#' @export
pretty_curr <- function(x, p = 0, curr = "") {
  x <- ifelse(is.na(x), NA, paste(curr, prettyNum(round(x, p), big.mark = ","), sep = " "))
  x
}

#' @export
pretty_perc <- function(x, p = 2) {
  x <- ifelse(is.na(x), NA, paste(round(x*100, p), '%', sep = " "))
  x
}

# pretty_json <- function(df) {
#   library(jsonlite)
#
#   x <- prettify(toJSON(as.data.frame(df)))
#   x
# }
