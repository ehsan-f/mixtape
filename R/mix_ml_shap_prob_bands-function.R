#' Categorize SHAP Values into Risk Impact Bands
#'
#' @description
#' Converts SHAP values into categorical risk bands based on their impact on
#' predicted probability. Designed for understanding negative feature impacts
#' (risk increases) in binary classification models.
#'
#' @param shap_values Numeric vector of SHAP values to categorize
#' @param model_bias Numeric. The model's baseline logit (bias term)
#' @param prob_thresholds Numeric vector of probability change thresholds.
#'   Default: c(0.025, 0.05, 0.10) creates 4 bands (high/medium/low risk + neutral)
#' @param labels Character vector of custom labels, or NULL for numeric codes (-3, -2, -1, 0).
#'   If provided, must match the number of bands (length of prob_thresholds + 1)
#'
#' @return Ordered factor with SHAP values categorized into risk bands
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Default: 4 bands with numeric labels
#' bands <- mix_ml_shap_prob_bands(shap_vals, model_bias = -2.5)
#'
#' # Custom thresholds (5 bands)
#' bands <- mix_ml_shap_prob_bands(shap_vals, model_bias = -2.5,
#'                                 prob_thresholds = c(0.01, 0.03, 0.05, 0.10))
#'
#' # Custom labels
#' bands <- mix_ml_shap_prob_bands(shap_vals, model_bias = -2.5,
#'                                 labels = c("High Risk", "Medium Risk", "Low Risk", "Neutral"))
#' }
mix_ml_shap_prob_bands <- function(shap_values,
                                   model_bias,
                                   prob_thresholds = c(0.025, 0.05, 0.10),
                                   labels = NULL) {

  #-- Calculate baseline probability from model bias
  baseline_logit <- model_bias
  baseline_prob <- 1 / (1 + exp(-baseline_logit))

  #-- Convert probability changes to SHAP thresholds
  shap_thresholds <- sapply(prob_thresholds, function(prob_change) {
    new_prob <- baseline_prob + prob_change
    new_prob <- pmax(0.001, pmin(0.999, new_prob))  # Keep within bounds
    new_logit <- -log(new_prob / (1 - new_prob))
    return(new_logit - baseline_logit)
  })

  #-- Create labels if not provided
  n_bands <- length(prob_thresholds) + 1
  if (is.null(labels)) {
    labels <- as.character(-(n_bands-1):0)
  }

  #-- Create bands
  cut(
    shap_values,
    breaks = c(-Inf, shap_thresholds, Inf),
    labels = labels,
    ordered_result = T,
    include.lowest = T
  )
}
