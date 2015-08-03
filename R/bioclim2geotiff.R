#' Bioclim to GeoTiff
#' 
#' This function wraps the bioclim function. It writes data to a geotiff file
#' in the working directory.
#' 
#' 
#' @param tmin a data.frame or matrix with 12 or 52 columns representing
#' monthly or weekly minimum temperature data; rows represent different
#' locations.
#' @param tmax a data.frame or matrix as with \code{tmin} representing maximum
#' temperature data.
#' @param prec a data.frame or matrix as with \code{tmin} representing
#' precipitation data.
#' @param tmean a data.frame or matrix as with \code{tmin} representing mean
#' temperature data.
#' @param vois a vector of values between 1 & 19 defining the bioclimatic
#' variables to be calculated; see details for variable description.
#' @param coordinates Coordinates of grid cells as output by CF_bbox_grid.
#' @param projection A Proj4 string for the spatial reference of the input
#' string.
#' @param year A year to use in labeling the output file.
#' @return A character vector of the files that were created.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @importFrom rgdal writeGDAL
#' @importFrom sp SpatialPixelsDataFrame SpatialPoints
#' @export
#' @examples
#' 
#' \dontrun{
#' data(bioclim2geotiff_example, package='climates')
#' fileNames<-bioclim2geotiff(tmax_data,tmin_data,prcp_data,tave_data,bioclims, coords_master, prj, year)
#' }
#' 
bioclim2geotiff<-function(tmax_data,tmin_data,prcp_data,tave_data,bioclims, coords_master, prj, year)
{
  #remove cells that are NaNs.
  mask<-!is.na(prcp_data[,1])
  masked_coords<-coords_master[mask,]
  tmax_data<-tmax_data[mask,]
  tmin_data<-tmin_data[mask,]
  prcp_data<-prcp_data[mask,]
  tave_data<-tave_data[mask,]
  # Run BioClim
  bioclim_out<-data.frame(bioclim(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, tmean=tave_data, bioclims))
  colnames(bioclim_out)<-paste('bioclim_',bioclims, sep='')
  fileNames<-c()
  for (bclim in names(bioclim_out))
  {
    file_name<-paste(bclim,'_',as.character(year),'.tif',sep='') 
    fileNames<-append(fileNames,file_name)
    writeGDAL(SpatialPixelsDataFrame(SpatialPoints(masked_coords, proj4string = CRS(prj)), bioclim_out[bclim], tolerance=0.0001),file_name)
  }
  return(fileNames)
}
