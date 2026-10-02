test_that('set_integer_to_numeric() casts integer columns to numeric', {
  df <- data.frame(a = 1:3, b = c(1.5, 2.5, 3.5), c = c('x', 'y', 'z'))
  result <- set_integer_to_numeric(df)

  expect_type(result$a, 'double')
  expect_equal(result$a, c(1, 2, 3))
  expect_identical(result$b, df$b)
  expect_identical(result$c, df$c)
})

test_that('set_integer_to_numeric() leaves integer-backed Dates alone', {
  df <- data.frame(n = 1:3)
  df$d <- seq(as.Date('2024-01-01'), by = 'day', length.out = 3)
  storage.mode(df$d) <- 'integer'
  result <- set_integer_to_numeric(df)

  expect_s3_class(result$d, 'Date')
  expect_equal(result$d, as.Date(c('2024-01-01', '2024-01-02', '2024-01-03')))
})
