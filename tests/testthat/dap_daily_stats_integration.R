library(ncdf4)
start <- "2090"
end <- "2092"
bbox_in<-c(-88,42,-88.25,42.25)
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

fileNames<-dap_daily_stats(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var,NetCDF_output)

expect_equal(length(fileNames),9)
for(file in fileNames) {
  data<-nc_open(file)
  print(file)
  check<-nc_open((paste('data/dap_daily_stats_bcca_p25xp25/',file,sep='')))
  expect_equivalent(ncvar_get(data),ncvar_get(check))
  expect_equivalent(data$dim$time$vals,check$dim$time$vals)
  expect_equivalent(data$dim$threshold$vals,check$dim$threshold$vals)
  expect_equivalent(data$dim$lon$vals,check$dim$lon$vals)
  expect_equivalent(data$dim$lat$vals,check$dim$lat$vals)
  expect_equivalent(data$dim$time_bounds$vals,check$dim$time_bounds$vals)
  nc_close(data)
  nc_close(check)
  file.remove(file)
}
