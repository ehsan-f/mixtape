#' Tune XGBoost model hyperparameters using cross-validation
#'
#' @description
#' Performs hyperparameter tuning for XGBoost classification models using k-fold
#' cross-validation with parallel processing. Returns the best hyperparameters and
#' lightweight CV models for ensemble predictions. Models are automatically slimmed
#' using the butcher package to reduce memory footprint.
#'
#' @param training_data Training dataset
#' @param recipe A tidymodels recipe object defining preprocessing steps
#' @param n_folds Number of cross-validation folds (default: 3)
#' @param n_workers Number of parallel workers (optional, auto-calculated if NULL)
#' @param learn_rate_values Learning rate values to test (default: c(0.01))
#' @param min_n_values Minimum node size values as proportion of training rows (default: c(0.003, 0.005))
#' @param alpha_values L1 regularization values to test (default: c(5, 8, 10))
#' @param lambda_values L2 regularization values to test (default: c(5, 10, 15))
#' @param trees_values Number of trees values to test (default: c(1500, 2000))
#' @param tree_depth_values Maximum tree depth values to test (default: c(8, 10, 12))
#' @param sample_size_values Row sampling proportion values to test (default: c(0.75))
#' @param colsample_bytree_values Column sampling proportion values to test (default: c(0.75))
#' @param early_stopping_rounds Number of rounds without improvement before stopping (default: 20)
#' @param seed Random seed for reproducibility (default: 123)
#' @param parallel_type Parallelization strategy: 'everything' or 'resamples' (default: 'everything')
#'
#' @return A list containing:
#' \describe{
#'   \item{cv_models}{Slimmed workflow objects for best hyperparameters (one per fold) for ensemble predictions}
#'   \item{best_params}{Best hyperparameter combination based on ROC AUC}
#'   \item{tune_results}{Top 20 hyperparameter combinations with performance metrics}
#'   \item{training_time}{Total time taken for hyperparameter tuning}
#' }
#'
#' @export
mix_ml_tunez <- function(training_data,
                         recipe,
                         n_folds = 3,
                         n_workers = NULL,

                         learn_rate_values = c(0.01),
                         min_n_values = c(0.003, 0.005),
                         alpha_values = c(5, 8, 10),
                         lambda_values = c(5, 10, 15),
                         trees_values = c(1500, 2000),
                         tree_depth_values = c(8, 10, 12),
                         sample_size_values = c(0.75),
                         colsample_bytree_values = c(0.75),

                         early_stopping_rounds = 20,
                         seed = 123,
                         parallel_type = 'everything') { #resamples

  #----- Load required packages
  require(tidymodels)
  require(future)
  require(doFuture)
  require(butcher)

  #----- Set future options for large datasets
  options(future.globals.maxSize = +Inf)

  #----- Calculate parallel configuration
  total_cores <- ls_config$parallel$max_cores

  # Auto-calculate workers if not provided
  if (is.null(n_workers)) {
    n_workers <- n_folds
    if (parallel_type == 'everything') {
      n_workers <- n_workers * 2  # Double workers for fold + hyperparameter parallelism
    }
  }

  # Allocate threads to each worker
  xgb_threads_per_worker <- floor(total_cores / n_workers)

  #----- Display parallelization setup
  message("\n=== Parallelization Setup ===")
  message("Total cores available: ", total_cores)
  message("Workers (parallel tasks): ", n_workers)
  message("Threads per XGBoost instance: ", xgb_threads_per_worker)
  message("Parallel strategy: ", parallel_type)
  message("=============================\n")

  #----- Initialize parallel cluster
  mix_cluster_make(n = n_workers, max_n = n_workers)
  doFuture::registerDoFuture()
  message("Backend: ", foreach::getDoParName(), " with ", foreach::getDoParWorkers(), " workers\n")

  #----- Create cross-validation folds
  data_folds <- vfold_cv(training_data, v = n_folds)
  v_nrows_training <- nrow(analysis(data_folds$splits[[1]]))

  #----- XGBoost model specification
  xgb_tune_spec <- boost_tree(
    mode = "classification",
    trees = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
    sample_size = tune(),
    min_n = tune()
  ) |>
    set_engine(
      'xgboost',
      alpha = tune(),                          # L1 regularization
      lambda = tune(),                         # L2 regularization
      colsample_bytree = tune(),               # Column sampling per tree
      counts = FALSE,
      early_stopping_rounds = early_stopping_rounds,
      eval_metric = "logloss",
      tree_method = 'hist',                    # Fast histogram-based algorithm
      nthread = xgb_threads_per_worker,
      verbose = 1
    )

  #----- Create hyperparameter grid
  # Convert min_n from proportion to row count
  ds_min_n_lookup <- data.frame(
    min_n_pct = min_n_values,
    min_n_rows = round(min_n_values * v_nrows_training)
  )

  xgb_grid <- expand.grid(
    learn_rate = learn_rate_values,
    min_n = ds_min_n_lookup$min_n_rows,
    alpha = alpha_values,
    lambda = lambda_values,
    trees = trees_values,
    tree_depth = tree_depth_values,
    sample_size = sample_size_values,
    colsample_bytree = colsample_bytree_values
  )

  total_tasks <- n_folds * nrow(xgb_grid)
  message("Hyperparameter combinations: ", nrow(xgb_grid))
  message("Total tasks (folds × params): ", total_tasks)
  message("Estimated batches: ", ceiling(total_tasks / n_workers), "\n")

  #----- Create workflow
  set.seed(seed)
  xgb_wf <- workflow() |>
    add_model(xgb_tune_spec) |>
    add_recipe(recipe)

  #----- Run hyperparameter tuning
  message("Starting tuning (this may take a while)...\n")
  v_start_time <- Sys.time()

  xgb_tune_results <- tune_grid(
    xgb_wf,
    resamples = data_folds,
    grid = xgb_grid,
    control = control_grid(
      verbose = TRUE,
      save_pred = TRUE,
      allow_par = TRUE,
      parallel_over = parallel_type,
      extract = function(x) butcher::butcher(x)  # Reduce memory by slimming workflows
    )
  )

  v_end_time <- Sys.time()
  time_taken <- difftime(v_end_time, v_start_time, units = "mins")
  message("\n=== Tuning Complete ===")
  message("Total time: ", round(as.numeric(time_taken), 2), " minutes")
  message("=======================\n")

  #----- Shutdown parallel cluster
  mix_cluster_stop()

  #----- Extract best results
  # Get top 20 hyperparameter combinations ranked by ROC AUC
  ds_results <- collect_metrics(xgb_tune_results) |>
    filter(.metric == 'roc_auc') |>
    arrange(desc(mean)) |>
    slice(1:20)

  # Select best hyperparameters
  ds_best_params <- select_best(xgb_tune_results, metric = "roc_auc")

  # Add original min_n percentage for reference
  ds_best_params <- ds_best_params |>
    left_join(ds_min_n_lookup, by = c("min_n" = "min_n_rows")) |>
    relocate(min_n_pct, .after = min_n)

  message("Best hyperparameters (by ROC AUC):")
  print(ds_best_params)

  #----- Extract CV models with best hyperparameters
  message("\nExtracting trained CV models for ensemble predictions...")

  # Filter for workflows matching best parameters (already slimmed by butcher)
  best_models_slim <- xgb_tune_results |>
    select(.extracts) |>
    unnest(cols = .extracts) |>
    filter(
      learn_rate == ds_best_params$learn_rate,
      min_n == ds_best_params$min_n,
      tree_depth == ds_best_params$tree_depth,
      trees == ds_best_params$trees,
      alpha == ds_best_params$alpha,
      lambda == ds_best_params$lambda,
      sample_size == ds_best_params$sample_size,
      colsample_bytree == ds_best_params$colsample_bytree
    ) |>
    pull(.extracts)

  message("Extracted ", length(best_models_slim), " slimmed CV models (one per fold)\n")

  #----- Return results
  return(list(
    cv_models = best_models_slim,
    best_params = ds_best_params,
    tune_results = ds_results,
    training_time = time_taken
  ))
}
