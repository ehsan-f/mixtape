#' Create training index
#'
#' @description
#' Creates a stratified train_index column using caret's createDataPartition.
#'
#' @param df Data frame to add index to (default: ds)
#' @param target Name of the target variable column for stratified sampling (default: 't_')
#' @param p Proportion of data to use for training (default: 0.7)
#'
#' @importFrom caret createDataPartition
#' @export
mix_train_index <- function(df = ds, target = 't_', p = 0.7) {

  #-- Create index
  train_ids <- createDataPartition(as.factor(df[[target]]), p = p, list = FALSE)

  df$train_index <- 0L
  df$train_index[train_ids] <- 1L

  return(df)
}


#' Split data into training and test sets
#'
#' @description
#' Splits a data frame into training and test sets using the train_index column.
#'
#' @param df Data frame with train_index column (default: ds)
#'
#' @importFrom dplyr filter
#' @export
mix_train_test_split <- function(df = ds) {

  #-- Split data
  list(
    train = df |> filter(train_index == 1L),
    test = df |> filter(train_index == 0L)
  )
}
