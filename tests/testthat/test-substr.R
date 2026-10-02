test_that('substr_right() takes characters from the right', {
  expect_equal(substr_right(c('abcdef', 'xyz'), 3), c('def', 'xyz'))
})

test_that('substr_left() takes characters from the left', {
  expect_equal(substr_left(c('abcdef', 'xyz'), 2), c('ab', 'xy'))
})
