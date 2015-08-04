library(climates)
context("Test CF_date_range function")

test_that("Example comes back as expected.", {
  data(CF_date_range_example, package='climates')
  check<-CF_date_range(time_units, time_dim, start, end)
  load('data/CF_date_range_example.xz') # loads a list named out
  expect_equal(check,out)
})


