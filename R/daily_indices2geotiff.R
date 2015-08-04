#' Daily Indices to GeoTiff
#' 
#' This function wraps the daily_indices function. It writes data to a geotiff
#' file in the working directory.
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
#' @param thresholds A named list with the following structure:\cr
#' thresholds=list(days_tmax_abv_thresh=c(),\cr days_tmin_blw_thresh=c(),\cr
#' days_prcp_abv_thresh=c(),\cr longest_run_tmax_abv_thresh=c(),\cr
#' longest_run_prcp_blw_thresh=c(),\cr growing_degree_day_thresh=c(),\cr
#' heating_degree_day_thresh=c(),\cr cooling_degree_day_thresh=c(),\cr
#' growing_season_lngth_thresh=c())\cr where thresholds are in celsius or mm
#' for temperature or precipitation respectively. Multiple thresholds can be
#' specified as a vector and entire statistics can be omitted.
#' @param coordinates Coordinates of grid cells as output by CF_bbox_grid.
#' @param projection A Proj4 string for the spatial reference of the input
#' string.
#' @param year A year to use in labeling the output file.
#' @return A character vector of the files that were created.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @export
#' @examples
#' 
#' \dontrun{
#' data(daily_indices2geotiff_example, package='climates')
#' fileNames<-daily_indices2geotiff(tmax_data, tmin_data, prcp_data, tave_data, thresholds, coords_master, prj, year,time_PCICt)
#' }
#' 
daily_indices2geotiff<-function(tmax_data,tmin_data,prcp_data,tave_data,thresholds,coords_master,prj,year,time_PCICt)
{
  tmax_data<-t(tmax_data)
  tmin_data<-t(tmin_data)
  prcp_data<-t(prcp_data)
  tave_data<-t(tave_data)
  #remove cells that are NaNs.
  mask<-!is.na(prcp_data[,1])
  masked_coords<-coords_master[mask,]
  tmax_data<-tmax_data[mask,]
  tmin_data<-tmin_data[mask,]
  prcp_data<-prcp_data[mask,]
  tave_data<-tave_data[mask,]
  tmax_data<-t(tmax_data)
  tmin_data<-t(tmin_data)
  prcp_data<-t(prcp_data)
  tave_data<-t(tave_data)
  # Run Stats
  statsout<-data.frame(daily_indices(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, tmean=tave_data, thresholds,time_PCICt))
  fileNames<-c()
  for (stat in colnames(statsout))
  {
    file_name<-paste(stat,'_',as.character(year),'.tif',sep='') 
    fileNames<-append(fileNames,file_name)
    writeGDAL(SpatialPixelsDataFrame(SpatialPoints(masked_coords, proj4string = CRS(prj)), statsout[stat], tolerance=0.0001),file_name)
  }
  return(fileNames)
}
