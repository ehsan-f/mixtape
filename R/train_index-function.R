#' Create a training/test split index
#'
#' @description
#' Creates an index column for splitting a data frame into training and test sets.
#' Uses stratified sampling based on a target variable.
#'
#' @param df Data frame to create index for (default: ds)
#' @param target Name of the target variable column (default: 't_')
#' @param p Proportion of data to use for training (default: 0.7)
#' @param env_var Environment to assign the result to (default: global environment)
#'
#' @importFrom caret createDataPartition
#' @import dplyr
#' @export
train_index <- function(df = ds, target = 't_', p = 0.7, env_var = NULL) {

  #-- Environment Variables
  # Current Env = environment(fun = NULL)
  if (is.null(env_var) | !is.environment(env_var)) {
    env_var <- globalenv()
  }

  #-- Data
  dx <- as.data.frame(df)

  #-- Train Index
  train_index <- createDataPartition(as.factor(dx[, target]),
                                    #times = 1,
                                    p = p,
                                    list = F)

  dx$train_index <- 0
  dx$train_index[train_index] <- 1

  #-- Assign
  dx <- as_tibble(dx)
  assign(deparse(substitute(df)), dx, envir = env_var)
}
