#' @export
tile_plot <- function (prob = 'p_',
                       y,
                       measure = NULL,
                       df_train = ds1,
                       df_test = ds2,
                       model = NULL,
                       model_var = 'X1',
                       n = 10,
                       nudge_train = 1.5,
                       nudge_test = 1.5,
                       generate_plot = T,
                       actual_colour = mix_palette$blue,
                       pred_colour = mix_palette$red)
{
  #-- Packages
  library(tidyverse)
  library(lemon)

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

  if (class(model)[1] == 'train') {
    df_train[,prob] <- predict(model, df_train, type = 'prob')[, model_var]

    if (!is.null(df_test)) {
      df_test[,prob] <- predict(model, df_test, type = 'prob')[, model_var]
    }
  }

  #================================================================================#

  #-- Assign Tiles
  df_train$Tile <- ntile(df_train[,prob], n)

  if (!is.null(df_test)) {
    df_test[, 'Tile'] <- 0
    df_test[df_test[, prob] %> =<% c(-Inf,
                                     max(df_train[df_train[,'Tile'] == 1, prob])),
            'Tile'] <- 1

    for (i in 2:(n-1)) {
      df_test[df_test[, prob] %> =<% c(max(df_train[df_train[,'Tile'] == i - 1, prob]),
                                       max(df_train[df_train[, 'Tile'] == i, prob])),
              'Tile'] <- i
    }
    df_test[df_test[, prob] %> =<% c(max(df_train[df_train[,'Tile'] == (n-1), prob]),
                                     Inf),
            'Tile'] <- n
  }
  #================================================================================#

  #----- Lift Tables
  #-- Train
  df_train$Actual_Train <- df_train[,y]
  df_train$Pred_Train <- df_train[,prob]

  lift_train <- df_train %>%
    mutate(Count = 1) %>%
    group_by(Tile) %>%
    summarise(
      Actual_Train = mean(Actual_Train),
      Pred_Train = mean(Pred_Train),
      Count = sum(Count)
    )

  if (!is.null(measure)) {
    df_train$Measure_Mean <- df_train[,measure]

    lift_train <- lift_train %>%
      left_join(
        (
          df_train %>%
            group_by(Tile) %>%
            summarise(Measure_Mean = mean(Measure_Mean))
        ),
        by = 'Tile'
      )

    a_train <- sum(lift_train$Measure_Mean[1:3])/sum(lift_train$Measure_Mean)
    b_train <- sum(lift_train$Measure_Mean[(nrow(lift_train)-2):nrow(lift_train)])/sum(lift_train$Measure_Mean)

  }

  print(lift_train)
  if (!is.null(measure)) {
    print(sprintf('Train Bottom 3 = %.*f', 3, a_train))
    print(sprintf('Train Top 3 = %.*f', 3, b_train))
  }

  #-- Test
  if (!is.null(df_test)) {
    df_test$Actual_Test <- df_test[,y]
    df_test$Pred_Test <- df_test[,prob]

    lift_test <- df_test %>%
      mutate(Count = 1) %>%
      group_by(Tile) %>%
      summarise(
        Actual_Test = mean(Actual_Test),
        Pred_Test = mean(Pred_Test),
        Count = sum(Count)
      )

    if (!is.null(measure)) {
      df_test$Measure_Mean <- df_test[,measure]

      lift_test <- lift_test %>%
        left_join(
          (
            df_test %>%
              group_by(Tile) %>%
              summarise(Measure_Mean = mean(Measure_Mean))
          ),
          by = 'Tile'
        )

      a_test <- sum(lift_test$Measure_Mean[1:3])/sum(lift_test$Measure_Mean)
      b_test <- sum(lift_test$Measure_Mean[(nrow(lift_test)-2):nrow(lift_test)])/sum(lift_test$Measure_Mean)
    }

    print(lift_test)
    if (!is.null(measure)) {
      print(sprintf('Train Bottom 3 = %.*f', 3, a_test))
      print(sprintf('Train Top 3 = %.*f', 3, b_test))
    }

  }

  #================================================================================#
  if (generate_plot == T) {
    #----- Lift Charts
    #-- Train
    gg_train <- lift_train %>%
      ggplot(aes(x = Tile)) +
      geom_line(aes(y = Actual_Train, color = 'Actual')) +
      geom_point(aes(y = Actual_Train, color = 'Actual'), size = 2) +

      geom_line(aes(y = Pred_Train, color = 'Predict')) +
      geom_point(aes(y = Pred_Train, color = 'Predict'), size = 2) +

      ggtitle('Lift Chart - Train', subtitle = measure) +
      ylab(prob) +

      scale_colour_manual(values = c(actual_colour, pred_colour)) +

      mix_theme() +
      theme(legend.title = element_blank())

    if (!is.null(measure)) {
      t_train <- lift_train$Actual_Train
      v_train <- t_train[2]
      w_train <- t_train[nrow(lift_train)-1]

      gg_train <- gg_train +
        geom_label(mapping = aes(2, v_train),
                   label = sprintf('Bottom 3 = %.*f%s', 1, a_train * 100, '%'),
                   color = mix_palette$green,
                   nudge_x = nudge_train + 0.3) +
        geom_label(mapping = aes(max(lift_train$Tile)-1, w_train),
                   label = sprintf('Top 3 = %.*f%s', 1, b_train * 100, '%'),
                   color = mix_palette$green,
                   nudge_x = - nudge_train)

    }

    reposition_legend(aplot = gg_train,  position = 'top left')

    #-- Test
    if (!is.null(df_test)) {
      gg_test <- lift_test %>%
        ggplot(aes(x = Tile)) +
        geom_line(aes(y = Actual_Test, color = 'Actual')) +
        geom_point(aes(y = Actual_Test, color = 'Actual'), size = 2) +

        geom_line(aes(y = Pred_Test, color = 'Predict')) +
        geom_point(aes(y = Pred_Test, color = 'Predict'), size = 2) +

        ggtitle('Lift Chart - Test', subtitle = measure) +
        ylab(prob) +

        scale_colour_manual(values = c(actual_colour, pred_colour)) +

        mix_theme() +
        theme(legend.title = element_blank())

      if (!is.null(measure)) {
        t_test <- lift_test$Actual_Test
        v_test <- t_test[2]
        w_test <- t_test[nrow(lift_test)-1]

        gg_test <- gg_test +
          geom_label(mapping = aes(2, v_test),
                     label = sprintf('Bottom 3 = %.*f%s', 1, a_test * 100, '%'),
                     color = mix_palette$green,
                     nudge_x = nudge_test + 0.3) +
          geom_label(mapping = aes(max(lift_test$Tile)-1, w_test),
                     label = sprintf('Top 3 = %.*f%s', 1, b_test * 100, '%'),
                     color = mix_palette$green,
                     nudge_x = - nudge_test)

      }

      reposition_legend(aplot = gg_test,  position = 'top left')
    }
  }

  #================================================================================#

  # Invisible Lists
  if (is.null(df_test)) {
    list(lift_train = lift_train, tiles_train = df_train[, 'Tile']) %>% invisible()
  }
  else if (!is.null(df_test)) {
    list(lift_train = lift_train, lift_lest = lift_test,
         tiles_train = df_train[, 'Tile'], tiles_test = df_test[, 'Tile']) %>% invisible()
  }
}
