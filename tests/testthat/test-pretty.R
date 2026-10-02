test_that('pretty_num() rounds and adds thousand separators', {
  expect_equal(pretty_num(1234567.89), '1,234,568')
  expect_equal(pretty_num(1234.56), '1,234.6')
  expect_equal(pretty_num(1234.56, p = 2), '1,234.56')
})

test_that('pretty_curr() adds the currency symbol', {
  expect_equal(pretty_curr(1500, curr = '£'), '£ 1,500')
  expect_equal(pretty_curr(1500.567, p = 2, curr = '$'), '$ 1,500.57')
})

test_that('pretty_curr() has no leading space when no currency symbol is given', {
  expect_equal(pretty_curr(1500), '1,500')
})

test_that('pretty_perc() converts a decimal to a percentage', {
  expect_equal(pretty_perc(0.75634), '75.63 %')
  expect_equal(pretty_perc(0.5, p = 0), '50 %')
})

test_that('pretty functions keep NA as NA', {
  expect_equal(pretty_num(c(1000, NA)), c('1,000', NA))
  expect_equal(pretty_curr(c(1000, NA), curr = '£'), c('£ 1,000', NA))
  expect_equal(pretty_perc(c(0.1, NA)), c('10 %', NA))
})
