library(climates)

context("Test daily2monthly function")

test_that("Example comes back as expected.", {
data(daily2monthly_example, package='climates')
check<-daily2monthly(tmax_data, time_indices, origin, cells)
load('data/daily2monthly_example.xz')
expect_equivalent(check[which(!is.na(check))],out[which(!is.na(out))])
})