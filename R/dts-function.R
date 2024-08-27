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
