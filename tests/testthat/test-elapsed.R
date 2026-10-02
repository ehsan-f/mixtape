test_that('elapsed_years() counts only completed years by default', {
  expect_equal(elapsed_years('2020-03-15', '2023-03-14'), 2)
  expect_equal(elapsed_years('2020-03-15', '2023-03-16'), 3)
})

test_that('elapsed_years() is not thrown off by leap years', {
  expect_equal(elapsed_years('2020-03-01', '2021-03-01'), 1)
  expect_equal(elapsed_years('2020-03-01', '2024-02-29'), 3)
  expect_equal(elapsed_years('2021-01-01', '2022-01-01'), 1)
  expect_equal(elapsed_years('2000-06-15', '2030-06-10'), 29)
})

test_that('elapsed_years() works on vectors of dates', {
  expect_equal(
    elapsed_years(c('2020-03-01', '2020-03-15'), c('2021-03-01', '2023-03-14')),
    c(1, 2)
  )
})

test_that("elapsed_years() with accuracy_type = 'sql' only compares calendar years", {
  expect_equal(elapsed_years('2020-12-31', '2021-01-01', accuracy_type = 'sql'), 1)
})

test_that('elapsed_months() counts only completed months by default', {
  expect_equal(elapsed_months('2023-01-15', '2023-03-14'), 1)
  expect_equal(elapsed_months('2023-01-15', '2023-03-15'), 2)
  expect_equal(elapsed_months('2022-11-15', '2023-02-20'), 3)
})

test_that("elapsed_months() with accuracy_type = 'sql' only compares calendar months", {
  expect_equal(elapsed_months('2023-01-15', '2023-03-14', accuracy_type = 'sql'), 2)
})

test_that('elapsed_weeks() counts completed 7-day periods', {
  expect_equal(elapsed_weeks('2024-01-01', '2024-01-14'), 1)
  expect_equal(elapsed_weeks('2024-01-01', '2024-01-15'), 2)
})

test_that('elapsed_weeks() with iso = T compares ISO week numbers', {
  expect_equal(elapsed_weeks('2024-01-01', '2024-01-15', iso = T), 2)
})

test_that('elapsed_days() counts days, including across a leap day', {
  expect_equal(elapsed_days('2024-02-28', '2024-03-01'), 2)
  expect_equal(elapsed_days('2024-01-01', '2024-01-01'), 0)
  expect_equal(elapsed_days('2024-01-10', '2024-01-01'), -9)
})
