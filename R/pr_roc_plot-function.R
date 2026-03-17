#' Create Precision-Recall curve plots for model evaluation
#'
#' @description
#' Creates Precision-Recall curve plots for model evaluation on training and optionally test datasets.
#' Calculates and displays PR-AUC. Particularly suited to imbalanced classification problems
#' where ROC-AUC can be misleadingly optimistic.
#'
#' @param prob Name of probability column (default: 'p_')
#' @param y Name of the target variable column
#' @param df_train Training dataset
#' @param df_test Test dataset (optional)
#' @param model Model object (optional)
#' @param generate_output Whether to generate output (default: TRUE)
#' @param train_colour Colour for training curve (default: mix_palette$blue)
#' @param test_colour Colour for test curve (default: mix_palette$red)
#'
#' @importFrom dplyr mutate bind_rows
#' @importFrom ggplot2 ggplot aes geom_line geom_hline ggtitle xlab ylab theme element_blank scale_color_manual
#' @importFrom ROCR prediction performance
#' @export
pr_roc_plot <- function(prob = 'p_',
                    y,
                    df_train,
                    df_test = NULL,
                    model = NULL,
                    generate_output = TRUE,
                    train_colour = mix_palette$blue,
                    test_colour = mix_palette$red) {

  #-- Data
  df_train <- as.data.frame(df_train)

  if (!is.null(df_test)) {
    df_test <- as.data.frame(df_test)
  }

  #-- Assign Predictions
  if (!is.null(model)) {
    if (class(model)[1] == "glm") {
      df_train[, prob] <- predict(model, df_train, type = "response")

      if (!is.null(df_test)) {
        df_test[, prob] <- predict(model, df_test, type = "response")
      }
    }

    if (class(model)[1] == "workflow") {
      df_train[, prob] <- predict(model, new_data = df_train, type = "prob")$.pred_1

      if (!is.null(df_test)) {
        df_test[, prob] <- predict(model, new_data = df_test, type = "prob")$.pred_1
      }
    }
  }

  #-- Helper: trapezoidal AUC from a ROCR performance object
  calc_prauc <- function(perf) {
    recall <- unlist(perf@x.values)
    precision <- unlist(perf@y.values)

    # Remove NAs introduced at boundary thresholds
    keep <- !is.na(precision) & !is.na(recall)
    recall <- recall[keep]
    precision <- precision[keep]

    # Trapezoidal integration
    sum(diff(recall) * (head(precision, -1) + tail(precision, -1)) / 2)
  }

  #----- Train
  pred_train <- prediction(df_train[, prob], df_train[, y])
  perf_train <- performance(pred_train, measure = "prec", x.measure = "rec")
  prauc_train <- calc_prauc(perf_train)

  # Baseline: prevalence rate of the positive class
  baseline <- mean(df_train[, y] == 1)

  pr_train <- data.frame(
    x = unlist(perf_train@x.values),
    y = unlist(perf_train@y.values)
  ) |>
    mutate(Data = 'Train')

  #----- Test
  if (!is.null(df_test)) {
    pred_test <- prediction(df_test[, prob], df_test[, y])
    perf_test <- performance(pred_test, measure = "prec", x.measure = "rec")
    prauc_test <- calc_prauc(perf_test)

    pr_test <- data.frame(
      x = unlist(perf_test@x.values),
      y = unlist(perf_test@y.values)
    ) |>
      mutate(Data = 'Test')
  } else {
    pr_test <- NULL
  }

  pr <- bind_rows(pr_train, pr_test) |>
    mutate(Data = factor(Data, levels = c('Train', 'Test')))

  #----- Plot
  gg_pr <- pr |>
    ggplot(aes(x = x, y = y, group = Data, colour = Data)) +

    # Baseline: a random classifier scores ~= prevalence rate
    geom_hline(yintercept = baseline, colour = mix_palette$green, lty = 2) +

    geom_line(lwd = 1, na.rm = TRUE) +

    ggtitle('Precision-Recall Curves') +
    xlab('Recall') +
    ylab('Precision') +

    mix_theme() +
    theme(
      legend.title    = element_blank(),
      legend.position = c(1, 1),
      legend.justification = c("right", "top")
    )

  if (is.null(df_test)) {
    gg_pr <- gg_pr +
      scale_color_manual(
        values = c(train_colour, test_colour),
        labels = c(sprintf("Train | PR-AUC = %.4f | Baseline = %.4f", prauc_train, baseline))
      )
  } else {
    gg_pr <- gg_pr +
      scale_color_manual(
        values = c(train_colour, test_colour),
        labels = c(
          sprintf("Train | PR-AUC = %.4f | Baseline = %.4f", prauc_train, baseline),
          sprintf("Test  | PR-AUC = %.4f | Baseline = %.4f", prauc_test,  baseline)
        )
      )
  }

  if (generate_output) {
    print(gg_pr)
  }

  #----- Output
  ls_output <- list(
    log_pr_train = perf_train,
    log_prauc_train = signif(prauc_train, digits = 4),
    log_baseline = signif(baseline, digits = 4),
    gg_pr = gg_pr
  )

  if (!is.null(df_test)) {
    ls_output$log_pr_test <- perf_test
    ls_output$log_prauc_test <- signif(prauc_test, digits = 4)
  }

  invisible(ls_output)
}
