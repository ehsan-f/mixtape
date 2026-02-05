#' Create lift charts using tile-based binning
#'
#' @description
#' Creates lift charts for model evaluation by binning predictions into tiles.
#' Works with training and test datasets and supports different model types.
#'
#' @param prob Name of probability column (default: 'p_')
#' @param y Name of the target variable column
#' @param measure Name of measure column for additional analysis (optional)
#' @param df_train Training dataset
#' @param df_test Test dataset
#' @param model Model object (optional)
#' @param n Number of tiles (default: 10)
#' @param nudge_train Position adjustment for training labels (default: 1.5)
#' @param nudge_test Position adjustment for test labels (default: 1.5)
#' @param generate_output Whether to generate output (default: TRUE)
#' @param actual_colour Colour for actual values (default: mix_palette$blue)
#' @param pred_colour Colour for predicted values (default: mix_palette$red)
#'
#' @importFrom dplyr ntile group_by summarise left_join sym
#' @importFrom ggplot2 ggplot aes geom_line geom_point ggtitle ylab scale_colour_manual theme element_blank annotate
#' @export
lift_chart <- function (prob = 'p_',
                        y,
                        measure = NULL,
                        df_train,
                        df_test,
                        model = NULL,
                        n = 10,
                        nudge_train = 1.5,
                        nudge_test = 1.5,
                        generate_output = T,
                        actual_colour = mix_palette$blue,
                        pred_colour = mix_palette$red)
{
  #-- Data
  df_train <- as.data.frame(df_train)

  if (!is.null(df_test)) {
    df_test <- as.data.frame(df_test)
  }


  #-- Assign Predictions
  if (class(model)[1] == 'glm') {
    df_train[,prob] <- predict(model, df_train, type = 'response')

    if (!is.null(df_test)) {
      df_test[,prob] <- predict(model, df_test, type = 'response')
    }
  }

  if (class(model)[1] == 'workflow') {
    df_train[,prob] <- predict(model, new_data = df_train, type = 'prob')$.pred_1

    if (!is.null(df_test)) {
      df_test[,prob] <- predict(model, new_data = df_test, type = 'prob')$.pred_1
    }
  }

  #================================================================================#

  #-- Assign tiles
  df_train$tile <- ntile(df_train[,prob], n)

  if (!is.null(df_test)) {
    v_breaks <- c(-Inf, sapply(1:(n-1), function(i) max(df_train[df_train$tile == i, prob])), Inf)
    df_test$tile <- cut(df_test[, prob], breaks = v_breaks, labels = FALSE)
  }

  #================================================================================#

  #----- Lift Tables
  #-- Train
  df_train$actual_train <- df_train[,y]
  df_train$pred_train <- df_train[,prob]

  lift_train <- df_train |>
    group_by(tile) |>
    summarise(
      actual_train = mean(actual_train),
      pred_train = mean(pred_train),
      records_vol = n()
    )

  v_lift_factor_train <- lift_train$actual_train[nrow(lift_train)] / lift_train$actual_train[1]

  if (!is.null(measure)) {
    lift_train <- lift_train |>
      left_join(
        df_train |>
          group_by(tile) |>
          summarise(measure_sum = sum(!!sym(measure))),
        by = 'tile'
      )

    bottom_3_train <- sum(lift_train$measure_sum[1:3]) / sum(lift_train$measure_sum)
    top_3_train <- sum(lift_train$measure_sum[(nrow(lift_train)-2):nrow(lift_train)]) / sum(lift_train$measure_sum)
  }

  if (generate_output == T) {
    print(lift_train)
    if (!is.null(measure)) {
      print(paste0('Train Bottom 3 = ', pretty_perc(bottom_3_train, 1)))
      print(paste0('Train Top 3 = ', pretty_perc(top_3_train, 1)))
    }
  }

  #-- Test
  if (!is.null(df_test)) {
    df_test$actual_test <- df_test[,y]
    df_test$pred_test <- df_test[,prob]

    lift_test <- df_test |>
      group_by(tile) |>
      summarise(
        actual_test = mean(actual_test),
        pred_test = mean(pred_test),
        records_vol = n()
      )

    v_lift_factor_test <- lift_test$actual_test[nrow(lift_test)] / lift_test$actual_test[1]

    if (!is.null(measure)) {
      lift_test <- lift_test |>
        left_join(
          df_test |>
            group_by(tile) |>
            summarise(measure_sum = sum(!!sym(measure))),
          by = 'tile'
        )

      bottom_3_test <- sum(lift_test$measure_sum[1:3]) / sum(lift_test$measure_sum)
      top_3_test <- sum(lift_test$measure_sum[(nrow(lift_test)-2):nrow(lift_test)]) / sum(lift_test$measure_sum)
    }

    if (generate_output == T) {
      print(lift_test)
      if (!is.null(measure)) {
        print(paste0('Test Bottom 3 = ', pretty_perc(bottom_3_test, 1)))
        print(paste0('Test Top 3 = ', pretty_perc(top_3_test, 1)))
      }
    }

  }

  #================================================================================#
  #----- Lift Charts
  #-- Train
  gg_train <- lift_train |>
    ggplot(aes(x = tile)) +
    geom_line(aes(y = actual_train, color = 'Actual')) +
    geom_point(aes(y = actual_train, color = 'Actual'), size = 2) +

    geom_line(aes(y = pred_train, color = 'Predict')) +
    geom_point(aes(y = pred_train, color = 'Predict'), size = 2) +

    ggtitle('Lift Chart - Train', subtitle = paste0('Lift = ', round(v_lift_factor_train, 2), 'x')) +
    ylab(prob) +

    scale_colour_manual(values = c(actual_colour, pred_colour)) +

    mix_theme() +
    theme(legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c("left", "top"))

  if (!is.null(measure)) {
    gg_train <- gg_train +
      annotate('label', x = 2 + nudge_train + 0.3, y = lift_train$actual_train[2],
               label = paste0('Bottom 3 = ', pretty_perc(bottom_3_train, 1)),
               color = mix_palette$green) +
      annotate('label', x = max(lift_train$tile) - 1 - nudge_train, y = lift_train$actual_train[nrow(lift_train) - 1],
               label = paste0('Top 3 = ', pretty_perc(top_3_train, 1)),
               color = mix_palette$green)
  }

  if (generate_output == T) {
    print(gg_train)
  }

  #-- Test
  if (!is.null(df_test)) {
    gg_test <- lift_test |>
      ggplot(aes(x = tile)) +
      geom_line(aes(y = actual_test, color = 'Actual')) +
      geom_point(aes(y = actual_test, color = 'Actual'), size = 2) +

      geom_line(aes(y = pred_test, color = 'Predict')) +
      geom_point(aes(y = pred_test, color = 'Predict'), size = 2) +

      ggtitle('Lift Chart - Test', subtitle = paste0('Lift = ', round(v_lift_factor_test, 2), 'x')) +
      ylab(prob) +

      scale_colour_manual(values = c(actual_colour, pred_colour)) +

      mix_theme() +
      theme(legend.title = element_blank(),
            legend.position = c(0, 1),
            legend.justification = c("left", "top"))

    if (!is.null(measure)) {
      gg_test <- gg_test +
        annotate('label', x = 2 + nudge_test + 0.3, y = lift_test$actual_test[2],
                 label = paste0('Bottom 3 = ', pretty_perc(bottom_3_test, 1)),
                 color = mix_palette$green) +
        annotate('label', x = max(lift_test$tile) - 1 - nudge_test, y = lift_test$actual_test[nrow(lift_test) - 1],
                 label = paste0('Top 3 = ', pretty_perc(top_3_test, 1)),
                 color = mix_palette$green)
    }

    if (generate_output == T) {
      print(gg_test)
    }
  }

  #================================================================================#

  #----- Invisible Lists
  ls_output <- list(
    lift_train = lift_train,
    lift_factor_train = v_lift_factor_train,
    tiles_train = df_train$tile,
    gg_train = gg_train
  )

  if (!is.null(df_test)) {
    ls_output$lift_test <- lift_test
    ls_output$lift_factor_test <- v_lift_factor_test
    ls_output$tiles_test <- df_test$tile
    ls_output$gg_test <- gg_test
  }

  invisible(ls_output)
}
