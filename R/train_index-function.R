#' @export
train_index <- function(df = ds, target = 't_', p = 0.7, env_var = NULL) {

  #-- Packages
  library(caret)
  library(dplyr)

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
