library(climates)
context("Test daily_indices function")

test_that("basic bcca integration test data comes back as expected.", {
load('data/dap_daily_stats_bcca_p25xp25/daily_indices_test.xz')
stats_test<-data.frame(daily_indices(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, tmean=tave_data, thresholds,time_PCICt))
expect_equal(stats_test,statsout)
})

