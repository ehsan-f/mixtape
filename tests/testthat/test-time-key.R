# weekdays() returns locale-dependent names, so these tests assume an English locale
# (the default on CI runners and the function's own defaults).

test_that('time_key() with df = NULL returns one row per day with the expected columns', {
  tk <- time_key(df = NULL, start = '2024-01-01', end = '2024-01-31')

  expect_equal(nrow(tk), 31)
  expect_equal(
    names(tk),
    c('day', 'day_no', 'week_start_date', 'week_no', 'calendar_week_no', 'month', 'month_no', 'year', 'weekend')
  )
  expect_equal(tk$day_no, 1:31)
  expect_true(all(tk$month == '2024-01'))
  expect_true(all(tk$month_no == 1))
})

test_that('time_key() ends weeks on end_of_week and flags weekend_days', {
  tk <- time_key(df = NULL, start = '2024-01-01', end = '2024-01-14')

  # 2024-01-06 is a Saturday, so week 1 is 1-6 Jan and week 2 starts on 7 Jan
  expect_equal(tk$week_no[tk$day == as.Date('2024-01-06')], 1)
  expect_equal(tk$week_no[tk$day == as.Date('2024-01-07')], 2)
  expect_equal(tk$week_start_date[tk$day == as.Date('2024-01-10')], as.Date('2024-01-07'))

  # default weekend_days are Friday and Saturday
  expect_equal(tk$weekend[tk$day %in% as.Date(c('2024-01-05', '2024-01-06'))], c(1, 1))
  expect_equal(tk$weekend[tk$day == as.Date('2024-01-07')], 0)
})

test_that('time_key() joins time features onto a data frame by its date column', {
  df <- data.frame(d = as.Date(c('2024-01-06', '2024-01-10')))
  tk <- time_key(df = df, x = 'd', start = '2024-01-01', end = '2024-01-31')

  expect_equal(nrow(tk), 2)
  expect_equal(tk$week_no, c(1, 2))
  expect_equal(tk$weekend, c(1, 0))
})
