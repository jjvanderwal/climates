#' Convenience utility to convert daily to monthly data.
#' 
#' Given data, an accompanying NetCDF coordinate variable, and the origin
#' (units) for the coordinate variable, this function will return monthly
#' resolution data.
#' 
#' This is a convenience function to help use daily data with theBioClim
#' utility.
#' 
#' @param daily_data A matrix of daily data values.
#' @param time The time indices that coorespond to the matrix of daily data.
#' @param origin The origin of the time coordinate variable.
#' @param cells The number of cells in the daily_data matrix. Optional.
#' @return The data converted to monthly resolution.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @export
#' @examples
#' 
#' 
#' data(daily2monthly_example, package='climates')
#' tmax_data<-daily2monthly(tmax_data, time_indices, origin, cells)
#' 
#' 
daily2monthly<-function(daily_data, time, origin_in, cells=NULL)
{
  if (is.null(cells)) cells<-ncol(daily_data)
  time<-chron(time,out.format=c(dates="year-m-day"),origin=origin_in)
  daily_data<-zoo(daily_data,time)
  daily_data<-aggregate(daily_data, as.yearmon, mean)
  monthlyData<-t(data.matrix(fortify.zoo(daily_data),cells)[1:12,2:(cells+1)])
  return(monthlyData)
}
