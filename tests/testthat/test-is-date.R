test_that('is_date() is TRUE for Date and datetime objects', {
  expect_true(is_date(as.Date('2024-01-01')))
  expect_true(is_date(as.POSIXct('2024-01-01 10:00:00', tz = 'UTC')))
  expect_true(is_date(as.POSIXlt('2024-01-01 10:00:00', tz = 'UTC')))
})

test_that('is_date() is FALSE for other types, including date strings', {
  expect_false(is_date('2024-01-01'))
  expect_false(is_date(20240101))
  expect_false(is_date(NULL))
})
