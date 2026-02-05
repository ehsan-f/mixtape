#' XGBoost Feature Selection
#'
#' @description
#' Performs feature selection using XGBoost importance scores. Trains an initial
#' model and selects features based on gain/frequency thresholds or grouped selection.
#'
#' @param df_train Data frame. Training dataset with target variable and features
#' @param df_features Data frame. Feature metadata with columns: variable, variable_group,
#'   neg_bal_feature, feature_measure
#' @param excl_features Character vector. Features to exclude manually
#' @param excl_features_regex Character. Regex pattern to exclude features
#' @param use_neg_bal_features Logical. Include negative balance features? Default: FALSE
#' @param use_amount_based_features Logical. Include amount-based features? Default: TRUE
#' @param grouped_selection Logical. Use grouped selection (top N per group)?
#'   If FALSE, uses gain/frequency thresholds. Default: TRUE
#' @param group_features_n Integer. Number of top features to select per group
#'   when grouped_selection = TRUE. Default: 5
#' @param target_var Character. Name of binary target variable. Default: "t_bad_y"
#' @param max_cores Integer. Maximum cores for parallel processing. Default: NULL
#'   (tries ls_config$parallel$max_cores, falls back to detectCores() - 2)
#'
#' @return List with:
#'   - ds_selected_features: Selected features with importance scores
#'   - v_important_features: Vector of selected feature names
#'   - ds_importance: All features with importance scores
#'   - model_auc: Training AUC of initial model
#'
#' @importFrom dplyr filter select left_join group_by arrange slice ungroup pull
#' @importFrom dplyr all_of
#' @importFrom recipes recipe
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom parsnip boost_tree set_engine fit extract_fit_engine
#' @importFrom tibble as_tibble
#' @importFrom janitor clean_names
#' @importFrom stats predict as.formula
#' @importFrom parallel detectCores
#' @importFrom xgboost xgb.importance
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with grouped selection
#' results <- mix_ml_feature_selection(df_train, df_features)
#'
#' # Threshold-based selection
#' results <- mix_ml_feature_selection(df_train, df_features,
#'                                     grouped_selection = FALSE)
#'
#' # Exclude specific features
#' results <- mix_ml_feature_selection(df_train, df_features,
#'                                     excl_features = c("feature1", "feature2"))
#' }
mix_ml_feature_selection <- function(df_train,
                                     df_features,
                                     excl_features = NULL,
                                     excl_features_regex = NULL,
                                     use_neg_bal_features = F,
                                     use_amount_based_features = T,
                                     grouped_selection = T,
                                     group_features_n = 5,
                                     target_var = "t_bad_y",
                                     max_cores = NULL) {

  #----- Initial Spec
  #-- Set max cores
  if (is.null(max_cores)) {
    max_cores <- tryCatch(
      ls_config$parallel$max_cores,
      error = function(e) parallel::detectCores(logical = TRUE) - 2
    )

    # If ls_config exists but max_cores is NULL, use fallback
    if (is.null(max_cores)) {
      max_cores <- parallel::detectCores(logical = TRUE) - 2
    }
  }

  #-- Model spec
  xgb_spec <- boost_tree(
    mode = "classification",
    trees = 2000,
    learn_rate = 0.01,
    tree_depth = 10,
    sample_size = 0.8,
    min_n = (0.005 * nrow(df_train)) |> round()
  ) |>
    set_engine(
      'xgboost',
      alpha = 1,
      lambda = 1,
      colsample_bytree = 0.8,
      counts = FALSE,
      early_stopping_rounds = 20,
      eval_metric = 'logloss',
      tree_method = 'hist',
      nthread = max_cores,
      verbose = F
    )

  #----- Recipe
  #-- Select features
  #- Neg bal features
  if (use_neg_bal_features == F) {
    message('Neg bal feature exclusion')
    df_features <- df_features |>
      filter(neg_bal_feature == 0 | is.na(neg_bal_feature))
  }

  #- Amount based features
  if (use_amount_based_features == F) {
    message('Amount based feature exclusion')
    df_features <- df_features |>
      filter(!feature_measure %in% c('amount'))
  }

  #- Manual feature exclusion
  if (!is.null(excl_features)) {
    message('Manual feature exclusion')
    df_features <- df_features |>
      filter(!(variable %in% excl_features))
  }

  #- Manual feature exclusion - regex
  if (!is.null(excl_features_regex)) {
    message('Manual feature exclusion - regex')
    df_features <- df_features |>
      filter(!grepl(pattern = excl_features_regex, x = variable))
  }

  #- Final features
  v_features <- df_features$variable

  #-- Create recipe
  recipe_formula <- as.formula(paste(target_var, "~ ."))
  ds_recipe <- df_train |>
    select(all_of(c(target_var, v_features))) |>
    recipe(recipe_formula)

  #----- Fit Model
  #-- Initial workflow
  xgb_wf <- workflow() |>
    add_recipe(ds_recipe) |>
    add_model(xgb_spec)

  #-- Fit model
  message('Model training')

  xgb_wf_train <- fit(xgb_wf, data = df_train)

  #-- AUC
  df_train$p_bad <- predict(xgb_wf_train, new_data = df_train, type = 'prob')$.pred_1

  ls_model_auc <- roc_plot(prob = 'p_bad', y = target_var,
                           df_train = df_train, df_test = NULL,
                           generate_output = F)

  #----- Important Features - XGBoost
  message('Important Feature - XGBoost')
  ds_importance <- xgboost::xgb.importance(model = extract_fit_engine(xgb_wf_train)) |>
    as_tibble() |>
    clean_names()

  ds_importance <- ds_importance |>
    left_join(
      df_features |>
        select(
          feature = variable,
          variable_group
        ),
      by = "feature"
    )


  #----- Feature Selection - XGBoost
  if (grouped_selection == T) {
    message('Grouped feature selection - XGBoost')

    ds_selected_features <- ds_importance |>
      group_by(variable_group) |>
      arrange(variable_group, desc(gain)) |>
      slice(1:group_features_n) |>
      ungroup() |>
      arrange(desc(gain))

    v_important_features <- ds_selected_features |>
      select(feature) |>
      pull()

  } else {
    ds_selected_features <- ds_importance |>
      filter(gain >= 0.001, frequency >= 0.001)

    v_important_features <- ds_selected_features |>
      select(feature) |>
      pull()
  }


  #----- Final Output
  ls_output <- list(
    ds_selected_features = ds_selected_features,
    v_important_features = v_important_features,
    ds_importance = ds_importance,
    model_auc = ls_model_auc$log_auc_train
  )

  return(ls_output)
}
