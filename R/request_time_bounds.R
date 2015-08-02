#' An OPeNDAP wrapper for the Climates function CF_date_range.
#'
#' This function takes an open ncdf4 data source, a start date and an end date.
#' It returns a named list per CF_date_range.
#'
#' @param ncdf4_handle ncdf4 object.
#' @param start a start year.
#' @param end an end year.
#' @return time_indices a named list per CF_date_range.
#' @export
#' @examples
#' Soon!
#' 
request_time_bounds<-function(ncdf4_handle,start,end)
{
  td<-get_time_dim(ncdf4_handle)
  time_units<-td$time_units
  time_dim<-td$time_dim
  return(CF_date_range(time_units, time_dim, start, end))
}