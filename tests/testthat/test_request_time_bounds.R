library(climates)

context("Test request_time_bounds function")

test_that("daymet time stuff is handled correctly.",{
  ncdf4_handle<-c()
  # daymet time units format.
  ncdf4_handle$dim$time$units<-"days since 1980-01-01 00:00:00 UTC"
  #Silly 0.5 time dimension is a real problem.
  ncdf4_handle$dim$time$vals<-sequence(365*2)-0.5
  time_out<-request_time_bounds(ncdf4_handle,"1980","1981")
  expect_that(time_out$origin[["month"]], equals(1))
  expect_that(time_out$origin[["year"]], equals(1980))
})

test_that("bcca time is handled correctly.",{
  ncdf4_handle<-c()
  ncdf4_handle$dim$time$units<-"days since 1950-01-01 00:00:00"
  ncdf4_handle$dim$time$vals<-sequence(365*2)-0.5+20454
  time_out<-request_time_bounds(ncdf4_handle,"2006","2007")
  expect_that(time_out$origin[["month"]], equals(1))
  expect_that(time_out$origin[["year"]], equals(1950))
})