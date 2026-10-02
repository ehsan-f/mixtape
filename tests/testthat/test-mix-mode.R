test_that('mix_mode() returns the most frequent value', {
  expect_equal(mix_mode(c(1, 2, 2, 3)), 2)
  expect_equal(mix_mode(c('a', 'b', 'b')), 'b')
})

test_that('mix_mode() returns all values tied for most frequent', {
  expect_equal(mix_mode(c('a', 'b', 'a', 'b', 'c')), c('a', 'b'))
})
