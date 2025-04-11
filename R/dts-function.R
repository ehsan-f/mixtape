#' Split data into training and test sets
#'
#' @description
#' Splits a data frame into training and test sets based on an index column.
#' Assigns the resulting data frames to the specified variables in the environment.
#'
#' @param df Data frame to split (default: ds)
#' @param x Name of the index column (default: "train_index")
#' @param df_train Name for the training data frame variable (default: ds1)
#' @param df_test Name for the test data frame variable (default: ds2)
#' @param env_var Environment to assign variables to (default: global environment)
#'
#' @export
dts <- function (df = ds, x = "train_index", df_train = ds1, df_test = ds2, env_var = NULL) {

  #-- Environment Variables
  # Current Env = environment(fun = NULL)
  if (is.null(env_var) | !is.environment(env_var)) {
    env_var <- globalenv()
  }

  #-- Dataframe
  df <- as.data.frame(df)

  #-- Train and Test
  train <- df[df[, x] == 1, ]
  test <- df[df[, x] == 0, ]

  #-- Assign
  assign(deparse(substitute(df_train)), train, envir = env_var)
  assign(deparse(substitute(df_test)), test, envir = env_var)
}
