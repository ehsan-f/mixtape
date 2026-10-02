test_that('range operators include or exclude the ends as named', {
  x <- c(0, 1, 5, 10, 11)

  expect_equal(x %>=<% c(1, 10), c(F, T, T, T, F))
  expect_equal(x %>= <% c(1, 10), c(F, T, T, F, F))
  expect_equal(x %> =<% c(1, 10), c(F, F, T, T, F))
  expect_equal(x %> <% c(1, 10), c(F, F, T, F, F))
})

test_that('%limit% caps values at both ends of the range', {
  expect_equal(c(-5, 0, 5, 10, 15) %limit% c(0, 10), c(0, 0, 5, 10, 10))
})

test_that('%bracket_max% returns the next highest bracket', {
  expect_equal(5 %bracket_max% c(10, 1, 20), 10)
  expect_equal(10 %bracket_max% c(10, 1, 20), 10)
})

test_that('%bracket_min% returns the next lowest bracket', {
  expect_equal(15 %bracket_min% c(10, 1, 20), 10)
  expect_equal(10 %bracket_min% c(10, 1, 20), 10)
})
