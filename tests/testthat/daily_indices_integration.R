library(climates)
library(PCICt) # Not sure why this doesn't load by default
library(testthat)
test_that("derivative portal thresholds work with bcca data and netcdf output", { 
  start <- "2098"
  end <- "2099"
  bbox_in<-c(-88,42,-89,43)
  thresholds=list(days_tmax_abv_thresh=c(32.2222,35,37.7778),
                  days_tmin_blw_thresh=c(-17.7778,-12.2222,0),
                  days_prcp_abv_thresh=c(25.4,50.8,76.2,101.6),
                  longest_run_tmax_abv_thresh=c(32.2222,35,37.7778),
                  longest_run_prcp_blw_thresh=c(76.2),
                  growing_degree_day_thresh=c(15.5556),
                  heating_degree_day_thresh=c(18.3333),
                  cooling_degree_day_thresh=c(18.3333),
                  growing_season_lngth_thresh=c(0))
  OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future"
  tmax_var  <- "BCCA_0-125deg_tasmax_day_ACCESS1-0_rcp45_r1i1p1"
  tmin_var <- "BCCA_0-125deg_tasmin_day_ACCESS1-0_rcp45_r1i1p1"
  prcp_var <- "BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1"
  tave_var <- NULL
  NetCDF_output=TRUE
  fileNames<-dap_daily_stats(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var, NetCDF_output)
  expect_that(length(fileNames), equals(9))
  expect_that("growing_degree_day.nc" %in% fileNames, is_true())
})

test_that("Degree Days Calculations", { 
  start <- "2098"
  end <- "2099"
  bbox_in<-c(-87,42,-89,43)
  thresholds=list(growing_degree_day_thresh=c(15.5556),
                  heating_degree_day_thresh=c(18.3333),
                  cooling_degree_day_thresh=c(18.3333),
                  growing_season_lngth_thresh=c(0))
  OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future"
  tmax_var  <- "BCCA_0-125deg_tasmax_day_ACCESS1-0_rcp45_r1i1p1"
  tmin_var <- "BCCA_0-125deg_tasmin_day_ACCESS1-0_rcp45_r1i1p1"
  prcp_var <- "BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1"
  tave_var <- NULL
  NetCDF_output=TRUE
  fileNames<-dap_daily_stats(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var, NetCDF_output, fill_nas=TRUE)
  expect_that(length(fileNames), equals(4))
})

test_that("derivative portal thresholds work with wicci data", { 
start <- "1961"
end <- "1961"
bbox_in<-c(-88,42,-89,43)
thresholds=list(days_tmax_abv_thresh=c(32.2222,35,37.7778),
                days_tmin_blw_thresh=c(-17.7778,-12.2222,0),
                days_prcp_abv_thresh=c(25.4,50.8,76.2,101.6),
                longest_run_tmax_abv_thresh=c(32.2222,35,37.7778),
                longest_run_prcp_blw_thresh=c(76.2),
                growing_degree_day_thresh=c(15.5556),
                heating_degree_day_thresh=c(18.3333),
                cooling_degree_day_thresh=c(18.3333),
                growing_season_lngth_thresh=c(0))
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/wicci/cmip3/20c3m"
tmax_var  <- "20c3m-cccma_cgcm3_1-tmax-01"
tmin_var <- "20c3m-cccma_cgcm3_1-tmin-01"
prcp_var <- "20c3m-cccma_cgcm3_1-prcp-01"
tave_var <- NULL
fileNames<-dap_daily_stats(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
print(fileNames)
print(length(fileNames))
expect_that(length(fileNames), equals(18))
expect_that("days_prcp_abv_50.8_C_1961.tif" %in% fileNames, is_true())
})

test_that("growing season length works with daymet data", { 
start <- "1980"
end <- "1980"
bbox_in<-c(-90.9,40.9,-91,41)
thresholds=list(growing_season_lngth_thresh=c(0))
OPeNDAP_URI<-"http://thredds.daac.ornl.gov/thredds/dodsC/daymet-agg/daymet-agg.ncml"
tmax_var <- "tmax"
tmin_var <- "tmin"
prcp_var <- "prcp"
tave_var <- NULL
fileNames<-dap_daily_stats(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(1))
expect_that("growing_season_lngth_0_C_1980.tif" %in% fileNames, is_true())
})

test_that("Two statistics work with hayhoe dataset for two years", { 
start <- "1960"
end <- "1961"
bbox_in<-c(-90,41,-90.5,41.5)
thresholds=list(days_tmax_abv_thresh=c(32.2222),
                growing_season_lngth_thresh=c(0))
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/dcp/conus"
tmax_var  <- "ccsm-a1b-tmax-NAm-grid"
tmin_var <- "ccsm-a1b-tmin-NAm-grid"
prcp_var <- "ccsm-a1fi-pr-NAm-grid"
tave_var <- NULL
fileNames<-dap_daily_stats(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(4))
expect_that("growing_season_lngth_0_C_1961.tif" %in% fileNames, is_true())
})

test_that("UofIMETDATA works with some statistics.", { 
start <- "1991"
end <- "1991"
bbox_in<-c(-88.5,42.5,-89,43)
thresholds=list(days_tmax_abv_thresh=c(32.2222,35,37.7778),
                days_tmin_blw_thresh=c(-17.7778,-12.2222,0),
                days_prcp_abv_thresh=c(25.4,50.8,76.2,101.6))
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/UofIMETDATA"
tmax_var  <- "max_air_temperature"
tmin_var <- "min_air_temperature"
prcp_var <- "precipitation_amount"
tave_var <- NULL
fileNames<-dap_daily_stats(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(10))
expect_that("days_tmax_abv_32.2222_C_1991.tif" %in% fileNames, is_true())
})

test_that("derivative portal thresholds work with bcca data", { 
  start <- "2090"
  end <- "2090"
  bbox_in<-c(-88,42,-88.5,42.5)
  thresholds=list(days_tmax_abv_thresh=c(32.2222,35,37.7778),
                  days_tmin_blw_thresh=c(-17.7778,-12.2222,0),
                  days_prcp_abv_thresh=c(25.4,50.8,76.2,101.6),
                  longest_run_tmax_abv_thresh=c(32.2222,35,37.7778),
                  longest_run_prcp_blw_thresh=c(76.2),
                  growing_degree_day_thresh=c(15.5556),
                  heating_degree_day_thresh=c(18.3333),
                  cooling_degree_day_thresh=c(18.3333),
                  growing_season_lngth_thresh=c(0))
  OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future"
  tmax_var  <- "BCCA_0-125deg_tasmax_day_ACCESS1-0_rcp45_r1i1p1"
  tmin_var <- "BCCA_0-125deg_tasmin_day_ACCESS1-0_rcp45_r1i1p1"
  prcp_var <- "BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1"
  tave_var <- NULL
  fileNames<-dap_daily_stats(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
  expect_that(length(fileNames), equals(18))
  expect_that("days_prcp_abv_50.8_C_2090.tif" %in% fileNames, is_true())
})
