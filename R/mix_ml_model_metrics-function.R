#' Extract ML model performance metrics
#'
#' @description
#' Computes key performance metrics for a trained classification model,
#' including AUC, lift charts, classification metrics (accuracy, precision, recall),
#' feature importance, and optimal probability cutoff.
#'
#' @param prob Name of the probability column
#' @param y Name of the target variable column
#' @param df_train Training dataset
#' @param df_test Test dataset
#' @param df_features Features data frame for feature importance join (default: ds_features)
#' @param model_type Type of model, used for feature importance extraction (default: 'xgb')
#' @param mod_xgb Trained tidymodels workflow object (default: NULL)
#' @param training_time Training time to store in output (default: NULL)
#'
#' @return A list containing auc, lift, tiles, p_optimum_cutoff, classification, feature_importance, and training_time
#'
#' @import dplyr
#' @import pROC
#' @import yardstick
#' @importFrom xgboost xgb.importance
#' @import janitor
#' @export
mix_ml_model_metrics <- function(prob, y,
                                 df_train,
                                 df_test,
                                 df_features = ds_features,
                                 model_type = 'xgb',
                                 mod_xgb = NULL,
                                 training_time = NULL) {

  #-- List object
  ls_model_metrics <- list()

  #-- Fix vars
  df_train[, 'p'] <- df_train[, prob]
  df_test[, 'p'] <- df_test[, prob]

  df_train[, 'y'] <- df_train[, y]
  df_test[, 'y'] <- df_test[, y]

  if (is.factor(df_train$y)) {
    df_train$y <- df_train$y |> as_nlevels()
    df_test$y <- df_test$y |> as_nlevels()
  }

  #----- ROC / AUC
  ls_model_auc <- roc_plot(prob = 'p', y = 'y',
                           df_train = df_train, df_test = df_test,
                           generate_output = F)

  ls_model_metrics$auc <- list(train = ls_model_auc$log_auc_train, test = ls_model_auc$log_auc_test)

  #----- Lift charts
  ls_model_lift <- lift_chart(prob = 'p', y = 'y', measure = 'y',
                              df_train = df_train,
                              df_test = df_test,
                              generate_output = F)

  ls_model_metrics$lift <- ls_model_lift[setdiff(names(ls_model_lift), c('tiles_train', 'tiles_test'))]
  ls_model_metrics$tiles <- ls_model_lift[c('tiles_train', 'tiles_test')]

  #----- Classification Metrics (Accuracy, Precision, Recall, Lift)
  #-- Probability cutoff
  roc_obj <- pROC::roc(df_train$y, df_train$p)
  ls_model_metrics$p_optimum_cutoff <- pROC::coords(roc_obj, "best", best.method = "closest.topleft")

  #-- Cutoff
  v_cutoff <- ls_model_metrics$p_optimum_cutoff$threshold

  #-- Metrics
  y_train_factor <- df_train$y |> as.factor()
  y_test_factor <- df_test$y |> as.factor()

  pred_train <- ifelse(df_train$p >= v_cutoff, 1, 0) |> as.factor()
  pred_test <- ifelse(df_test$p >= v_cutoff, 1, 0) |> as.factor()

  ls_model_metrics$classification <- list(
    train = tibble(
      accuracy = accuracy_vec(y_train_factor, pred_train),
      precision = precision_vec(y_train_factor, pred_train, event_level = "second"),
      recall = recall_vec(y_train_factor, pred_train, event_level = "second"),
      lift = ls_model_metrics$lift$lift_factor_train
    ),
    test = tibble(
      accuracy = accuracy_vec(y_test_factor, pred_test),
      precision = precision_vec(y_test_factor, pred_test, event_level = "second"),
      recall = recall_vec(y_test_factor, pred_test, event_level = "second"),
      lift = ls_model_metrics$lift$lift_factor_test
    )
  )

  #----- Feature importance
  if (model_type == 'xgb') {
    ls_model_metrics$feature_importance <- xgboost::xgb.importance(model = extract_fit_engine(mod_xgb)) |>
      as_tibble() |>
      clean_names() |>
      left_join(
        (
          df_features |>
            select(feature = variable, variable_group)
        )
      )
  }

  #----- Training Time
  if (!is.null(training_time)) {
    ls_model_metrics$training_time <- training_time
  }

  #-- Export
  return(ls_model_metrics)
}
