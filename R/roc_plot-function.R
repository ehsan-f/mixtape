#' @export
roc_plot <- function (prob = 'p_',
                      y = "t_Paid",
                      df_train = ds1,
                      df_test = ds2,
                      model = NULL,
                      model_var = 'X1',
                      generate_plot = T,
                      train_colour = mix_palette$blue,
                      test_colour = mix_palette$red)
{

  #-- Packages
  library(dplyr)
  library(ROCR)
  library(lemon)
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

  if (class(model)[1] == "train") {
    df_train[,prob] <- predict(model, df_train, type = "prob")[, model_var]

    if (!is.null(df_test)) {
      df_test[,prob] <- predict(model, df_test, type = 'prob')[, model_var]
    }
  }

  #----- Data
  #-- Train
  pred_train <- prediction(df_train[, prob], df_train[,y])
  perf_train <- perform(pred_train, measure = "tpr",x.measure = "fpr")
  auc_perf_train <- perform(pred_train, measure = "auc")

  roc_train <- data.frame('x' = unlist(perf_train@x.values), 'y' = unlist(perf_train@y.values)) %>%
    mutate(Data = 'Train')

  #-- Test
  if (!is.null(df_test)) {
    pred_test <- prediction(df_test[, prob], df_test[,y])
    perf_test <- perform(pred_test, measure = "tpr",x.measure = "fpr")
    auc_perf_test <- perform(pred_test, measure = "auc")

    roc_test <- data.frame('x' = unlist(perf_test@x.values), 'y' = unlist(perf_test@y.values)) %>%
      mutate(Data = 'Test')
  } else if (is.null(df_test)) {
    roc_test <- NULL
  }

  roc <- bind_rows(roc_train, roc_test) %>%
    mutate(Data = factor(Data, levels = c('Train', 'Test')))

  #----- Plots
  if (generate_plot == T) {
    gg_roc <- roc %>%
      ggplot(aes(x = x, y = y, group = Data, colour = Data)) +

      geom_abline(slope = 1, intercept = 0, color = mix_palette$green, lty = 2) +

      geom_line(lwd = 1) +

      ggtitle('ROC Curves') + xlab('False Positive Rate') + ylab('True Positive Rate') +

      mix_theme() +
      theme(legend.title = element_blank())

    if (is.null(df_test)) {
      gg_roc <- gg_roc +
        scale_color_manual(values = c(train_colour, test_colour),
                           labels = c(sprintf("Train | AUC = %.4f | Gini = %.4f", auc_perf_train@y.values[[1]], auc_perf_train@y.values[[1]]*2 - 1)))
    } else if (!is.null(df_test)) {
      gg_roc <- gg_roc +
        scale_color_manual(values = c(train_colour, test_colour),
                           labels = c(sprintf("Train | AUC = %.4f | Gini = %.4f", auc_perf_train@y.values[[1]], auc_perf_train@y.values[[1]]*2 - 1),
                                      sprintf("Test  | AUC = %.4f | Gini = %.4f", auc_perf_test@y.values[[1]], auc_perf_test@y.values[[1]]*2 - 1)))
    }

    reposition_legend(aplot = gg_roc,  position = 'top left')
  }

  #----- Output
  if (is.null(df_test)) {
    print(auc_perf_train@y.values)

    list(log_roc_train = perf_train,
         log_auc_train = signif(as.numeric(auc_perf_train@y.values), digits = 4)) %>% invisible()
  } else if (!is.null(df_test)) {
    print(auc_perf_train@y.values)
    print(auc_perf_test@y.values)

    list(log_roc_train = perf_train,
         log_auc_train = signif(as.numeric(auc_perf_train@y.values), digits = 4),
         log_roc_test = perf_test,
         log_auc_test = signif(as.numeric(auc_perf_test@y.values), digits = 4)) %>% invisible()
  }
}
