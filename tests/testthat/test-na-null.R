test_that('if_na() replaces NA values', {
  expect_equal(if_na(c(1, NA, 3), 0), c(1, 0, 3))
  expect_equal(if_na(c('a', NA), 'missing'), c('a', 'missing'))
})

test_that('if_na() with na = F replaces the given values instead', {
  expect_equal(if_na(c('a', 'b', 'x'), 'z', na = F, value = c('x', 'b')), c('a', 'z', 'z'))
})

test_that('if_null() replaces empty and NULL-like strings', {
  expect_equal(
    if_null(c('a', '', ' ', 'NULL', 'null', 'b')),
    c('a', NA, NA, NA, NA, 'b')
  )
  expect_equal(if_null(c('a', ''), 'missing'), c('a', 'missing'))
})

test_that('if_null() replaces infinite values', {
  expect_equal(if_null(c(1, Inf, -Inf)), c(1, NA, NA))
})

test_that('if_null() returns the replacement for NULL input', {
  expect_true(is.na(if_null(NULL)))
  expect_equal(if_null(NULL, 0), 0)
})
