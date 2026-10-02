test_that('nested_tbl_build() packs each table into a list-column of a one-row tibble', {
  ls_tbl <- list(
    monthly_base = data.frame(month = c('2024-01', '2024-02'), amount = c(10, 20)),
    tag_emergence = data.table::data.table(serene_tag_final = 'groceries')
  )
  result <- nested_tbl_build(ls_tbl)

  expect_s3_class(result, 'tbl_df')
  expect_equal(nrow(result), 1)
  expect_named(result, c('monthly_base', 'tag_emergence'))
})

test_that('nested_tbl_build() converts every table to a plain tibble', {
  result <- nested_tbl_build(list(a = data.table::data.table(x = 1:2)))

  expect_s3_class(result$a[[1]], 'tbl_df')
  expect_false(inherits(result$a[[1]], 'data.table'))
})

test_that('nested_tbl_build() returns an empty tibble for an empty list', {
  result <- nested_tbl_build(list())

  expect_s3_class(result, 'tbl_df')
  expect_equal(ncol(result), 0)
})

test_that('nested_tbl_extract() returns the table stored in a list-column', {
  df <- data.frame(user_id = 1)
  df$monthly_base <- list(data.frame(month = c('2024-01', '2024-02'), amount = c(10, 20)))
  result <- nested_tbl_extract(df, 'monthly_base')

  expect_s3_class(result, 'data.frame')
  expect_equal(result$amount, c(10, 20))
})

test_that('nested_tbl_extract() round-trips with nested_tbl_build()', {
  df_base <- data.frame(month = c('2024-01', '2024-02'), amount = c(10, 20))
  result <- nested_tbl_extract(nested_tbl_build(list(monthly_base = df_base)), 'monthly_base')

  expect_equal(as.data.frame(result), df_base)
})

test_that('nested_tbl_extract() returns a non-list column as-is and NULL for a missing column', {
  df <- data.frame(user_id = c(1, 2))

  expect_equal(nested_tbl_extract(df, 'user_id'), c(1, 2))
  expect_null(nested_tbl_extract(df, 'missing'))
})
