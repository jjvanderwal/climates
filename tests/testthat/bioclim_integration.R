library(climates)
library(testthat)
test_that("all bioclims work with wicci data", { 
start <- "1961"
end <- "1962"
bbox_in<-c(-88,42,-89,43)
bioclims<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/wicci/cmip3/20c3m"
tmax_var  <- "20c3m-cccma_cgcm3_1-tmax-01"
tmin_var <- "20c3m-cccma_cgcm3_1-tmin-01"
prcp_var <- "20c3m-cccma_cgcm3_1-prcp-01"
tave_var <- NULL
fileNames<-dap_bioclim(start,end,bbox_in,bioclims,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(38))
expect_that("bioclim_14_1962.tif" %in% fileNames, is_true())
})

test_that("a bioclim works with daymet data", { 
start <- "1980"
end <- "1980"
bbox_in<-c(-90.9,40.9,-91,41)
bioclims<-c(1)
OPeNDAP_URI<-"http://thredds.daac.ornl.gov/thredds/dodsC/daymet-agg/daymet-agg.ncml"
tmax_var <- "tmax"
tmin_var <- "tmin"
prcp_var <- "prcp"
tave_var <- NULL
fileNames<-dap_bioclim(start,end,bbox_in,bioclims,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(1))
expect_that("bioclim_1_1980.tif" %in% fileNames, is_true())
})

test_that("Two BioClims work with hayhoe dataset for two years", { 
start <- "1960"
end <- "1962"
bbox_in<-c(-90,41,-90.5,41.5)
bioclims<-c(1,2)
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/dcp/conus"
tmax_var  <- "ccsm-a1b-tmax-NAm-grid"
tmin_var <- "ccsm-a1b-tmin-NAm-grid"
prcp_var <- "ccsm-a1fi-pr-NAm-grid"
tave_var <- NULL
fileNames<-dap_bioclim(start,end,bbox_in,bioclims,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(6))
expect_that("bioclim_2_1962.tif" %in% fileNames, is_true())
})

test_that("PRISM works with 7 bioclims.", { 
start <- "1950"
end <- "1950"
bbox_in<-c(-87,41,-89,43)
bioclims<-c(1,2,3,4,5,6,7)
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/prism"
tmax_var  <- "tmx"
tmin_var <- "tmn"
prcp_var <- "ppt"
tave_var <- NULL
fileNames<-dap_bioclim(start,end,bbox_in,bioclims,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(7))
expect_that("bioclim_7_1950.tif" %in% fileNames, is_true())
})

test_that("UofIMETDATA works with 7 bioclims.", { 
start <- "1990"
end <- "1991"
bbox_in<-c(-88.5,42.5,-89,43)
bioclims<-c(1,2,3,4,5,6,7)
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/UofIMETDATA"
tmax_var  <- "max_air_temperature"
tmin_var <- "min_air_temperature"
prcp_var <- "precipitation_amount"
tave_var <- NULL
fileNames<-dap_bioclim(start,end,bbox_in,bioclims,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(14))
expect_that("bioclim_7_1991.tif" %in% fileNames, is_true())
})

#To Reproduce NULL bioclim 3.
test_that("PRISM to reproduce a bug found in Climates, should have been fixed", {
start <- "2011"
end <- "2012"
bbox_in<-c(-104.21872231305537,37.90511126546139,-115.402527071724,48.883528290648364)
bioclims<-c(1,2,3,4,5)
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/prism"
tmax_var  <- "tmx"
tmin_var <- "tmn"
prcp_var <- "ppt"
tave_var <- NULL
fileNames<-dap_bioclim(start,end,bbox_in,bioclims,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
expect_that(length(fileNames), equals(10))
expect_that("bioclim_5_2012.tif" %in% fileNames, is_true())
})