#' Create ROC curve plots for model evaluation
#'
#' @description
#' Creates ROC curve plots for model evaluation on training and optionally test datasets.
#' Calculates and displays AUC and Gini coefficients.
#'
#' @param prob Name of probability column (default: 'p_')
#' @param y Name of the target variable column
#' @param df_train Training dataset (default: ds1)
#' @param df_test Test dataset (default: ds2)
#' @param model Model object (optional)
#' @param generate_output Whether to generate output (default: TRUE)
#' @param train_colour Colour for training curve (default: mix_palette$blue)
#' @param test_colour Colour for test curve (default: mix_palette$red)
#'
#' @export
roc_plot <- function (prob = 'p_',
                      y,
                      df_train = ds1,
                      df_test = ds2,
                      model = NULL,
                      generate_output = T,
                      train_colour = mix_palette$blue,
                      test_colour = mix_palette$red)
{

  #-- Packages
  library(tidyverse)
  library(ROCR)
  perform <- ROCR::performance

  #-- Data
  df_train <- as.data.frame(df_train)

  if (!is.null(df_test)) {
    df_test <- as.data.frame(df_test)
  }

  #-- Assign Predictions
  if (class(model)[1] == "glm") {
    df_train[,prob] <- predict(model, df_train, type = "response")

    if (!is.null(df_test)) {
      df_test[,prob] <- predict(model, df_test, type = 'response')
    }
  }

  if (class(model)[1] == "workflow") {
    df_train[,prob] <- predict(model, new_data = df_train, type = 'prob')$.pred_1

    if (!is.null(df_test)) {
      df_test[,prob] <- predict(model, new_data = df_test, type = 'prob')$.pred_1
    }
  }

  #----- Data
  #-- Train
  pred_train <- prediction(df_train[, prob], df_train[,y])
  perf_train <- perform(pred_train, measure = "tpr", x.measure = "fpr")
  auc_perf_train <- perform(pred_train, measure = "auc")

  roc_train <- data.frame('x' = unlist(perf_train@x.values), 'y' = unlist(perf_train@y.values)) |>
    mutate(Data = 'Train')

  #-- Test
  if (!is.null(df_test)) {
    pred_test <- prediction(df_test[, prob], df_test[,y])
    perf_test <- perform(pred_test, measure = "tpr", x.measure = "fpr")
    auc_perf_test <- perform(pred_test, measure = "auc")

    roc_test <- data.frame('x' = unlist(perf_test@x.values), 'y' = unlist(perf_test@y.values)) |>
      mutate(Data = 'Test')
  } else {
    roc_test <- NULL
  }

  roc <- bind_rows(roc_train, roc_test) |>
    mutate(Data = factor(Data, levels = c('Train', 'Test')))

  #----- Plots
  gg_roc <- roc |>
    ggplot(aes(x = x, y = y, group = Data, colour = Data)) +

    geom_abline(slope = 1, intercept = 0, color = mix_palette$green, lty = 2) +

    geom_line(lwd = 1) +

    ggtitle('ROC Curves') + xlab('False Positive Rate') + ylab('True Positive Rate') +

    mix_theme() +
    theme(legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c("left", "top"))

  if (is.null(df_test)) {
    gg_roc <- gg_roc +
      scale_color_manual(values = c(train_colour, test_colour),
                         labels = c(sprintf("Train | AUC = %.4f | Gini = %.4f", auc_perf_train@y.values[[1]], auc_perf_train@y.values[[1]]*2 - 1)))
  } else {
    gg_roc <- gg_roc +
      scale_color_manual(values = c(train_colour, test_colour),
                         labels = c(sprintf("Train | AUC = %.4f | Gini = %.4f", auc_perf_train@y.values[[1]], auc_perf_train@y.values[[1]]*2 - 1),
                                    sprintf("Test  | AUC = %.4f | Gini = %.4f", auc_perf_test@y.values[[1]], auc_perf_test@y.values[[1]]*2 - 1)))
  }

  if (generate_output == T) {
    print(gg_roc)
  }

  #----- Output
  ls_output <- list(
    log_roc_train = perf_train,
    log_auc_train = signif(as.numeric(auc_perf_train@y.values), digits = 4),
    gg_roc = gg_roc
  )

  if (!is.null(df_test)) {
    ls_output$log_roc_test <- perf_test
    ls_output$log_auc_test <- signif(as.numeric(auc_perf_test@y.values), digits = 4)
  }

  invisible(ls_output)
}
