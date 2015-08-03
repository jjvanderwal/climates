library(climates)
library(rgdal)
context("Test bioclim2geotiff function")

test_that("Example comes back as expected.", {
  data(bioclim2geotiff_example, package='climates')
  fileNames<-bioclim2geotiff(tmax_data,tmin_data,prcp_data,tave_data,bioclims, coords_master, prj, year)
  expect_equal(length(fileNames),7)
  for(file in fileNames) {
    out<-readGDAL(file,silent=TRUE)
    check<-readGDAL(paste('data/bioclim2geotiff_example/',file,sep=''),silent=TRUE)
    expect_equivalent(out,check)
    file.remove(file)
  }
})



