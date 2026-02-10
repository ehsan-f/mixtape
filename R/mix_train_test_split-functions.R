#' Create training index
#'
#' @description
#' Creates a stratified train_index column using caret's createDataPartition.
#' Optionally splits at the group level (e.g., by user_id) to prevent the same
#' group from appearing in both train and test sets.
#'
#' @param df Data frame to add index to
#' @param target Name of the target variable column for stratified sampling (default: 't_')
#' @param p Proportion of data to use for training (default: 0.7)
#' @param group Optional column name for group-level splitting (e.g., 'user_id').
#'   When provided, stratification is done on a group-level "ever bad" flag (max of target per group),
#'   and all rows for a group are assigned to the same split.
#'
#' @importFrom caret createDataPartition
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @export
mix_train_index <- function(df, target = 't_', p = 0.7, group = NULL) {

  if (is.null(group)) {

    #-- Row-level split (original behavior)
    train_ids <- createDataPartition(as.factor(df[[target]]), p = p, list = FALSE)

    df$train_index <- 0L
    df$train_index[train_ids] <- 1L

  } else {

    #-- Group-level split
    df_groups <- df |>
      dplyr::group_by(.data[[group]]) |>
      dplyr::summarise(target_group = max(.data[[target]], na.rm = TRUE), .groups = 'drop')

    train_ids <- createDataPartition(as.factor(df_groups$target_group), p = p, list = FALSE)

    train_groups <- df_groups[[group]][train_ids]

    df$train_index <- as.integer(df[[group]] %in% train_groups)

  }

  return(df)
}


#' Split data into training and test sets
#'
#' @description
#' Splits a data frame into training and test sets using the train_index column.
#'
#' @param df Data frame with train_index column
#'
#' @importFrom dplyr filter
#' @export
mix_train_test_split <- function(df) {

  #-- Split data
  list(
    train = df |> filter(train_index == 1L),
    test = df |> filter(train_index == 0L)
  )
}
