#' Create and apply training/test split
#'
#' @description
#' Creates or applies a training/test split index using data.table for performance.
#' Can either create a new stratified split or use an existing train_index column.
#' Optionally splits the data into separate training and test datasets.
#'
#' @param df Data frame to split (default: ds)
#' @param target Name of the target variable column for stratified sampling (default: 't_')
#' @param p Proportion of data to use for training (default: 0.7)
#' @param create_index Whether to create a new train_index (TRUE) or use existing (FALSE) (default: TRUE)
#' @param split_data Whether to split into separate train/test dataframes (default: FALSE)
#' @param df_train Name for the training dataframe variable if split_data=TRUE (default: ds1)
#' @param df_test Name for the test dataframe variable if split_data=TRUE (default: ds2)
#' @param env_var Environment to assign variables to (default: global environment)
#'
#' @export
mix_train_test_split <- function(df = ds,
                                 target = 't_',
                                 p = 0.7,
                                 create_index = F,
                                 split_data = T,
                                 df_train = ds1,
                                 df_test = ds2,
                                 env_var = NULL) {

  #-- Packages
  library(data.table)
  library(caret)

  #-- Environment
  if (is.null(env_var)) env_var <- globalenv()

  #-- Convert to data.table
  dt <- as.data.table(df)

  #-- Create index
  if (create_index == T) {
    train_ids <- createDataPartition(as.factor(dt[[target]]), p = p, list = F)
    dt[, train_index := 0L]
    dt[train_ids, train_index := 1L]

    #-- Assign updated dataframe
    assign(deparse(substitute(df)), dt, envir = env_var)
  }

  #-- Split data
  if (split_data == T) {
    assign(deparse(substitute(df_train)), dt[train_index == 1], envir = env_var)
    assign(deparse(substitute(df_test)), dt[train_index == 0], envir = env_var)
  }
}
