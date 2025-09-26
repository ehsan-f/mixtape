#' Create training index
#'
#' @description
#' Creates a stratified train_index column using caret's createDataPartition.
#'
#' @param df Data frame to add index to (default: ds)
#' @param target Name of the target variable column for stratified sampling (default: 't_')
#' @param p Proportion of data to use for training (default: 0.7)
#'
#' @export
mix_train_index <- function(df = ds, target = 't_', p = 0.7) {

  #-- Packages
  library(data.table)
  library(caret)

  #-- Convert to data.table
  dt <- df
  setDT(dt)

  #-- Create index
  train_ids <- createDataPartition(as.factor(dt[[target]]), p = p, list = F)
  set(dt, j = "train_index", value = 0L)
  set(dt, i = train_ids, j = "train_index", value = 1L)

  return(dt)
}


#' Split data into training and test sets
#'
#' @description
#' Splits a data frame into training and test sets using the train_index column.
#'
#' @param df Data frame with train_index column (default: ds)
#'
#' @export
mix_train_test_split <- function(df = ds) {

  #-- Packages
  library(data.table)

  #-- Convert to data.table
  dt <- df
  setDT(dt)

  #-- Split data
  list(
    train = dt[train_index == 1],
    test = dt[train_index == 0]
  )
}
