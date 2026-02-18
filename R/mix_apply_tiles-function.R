#' Apply training tile cutoffs to a new dataset
#'
#' @description
#' Assigns tiles to a probability vector using the breakpoints derived from the
#' training dataset. Use this to consistently tile any scored dataset (e.g.
#' production scoring runs) with the same thresholds used during model evaluation.
#'
#' @param prob Numeric vector of model probabilities to bin
#' @param tile_breaks Break points from training data, typically from \code{model_metrics$tiles$tile_breaks}
#'
#' @return Integer vector of tile assignments (1 = lowest, n = highest)
#'
#' @export
mix_apply_tiles <- function(prob, tile_breaks) {
  cut(prob, breaks = tile_breaks, labels = FALSE)
}
