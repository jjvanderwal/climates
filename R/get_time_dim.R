#' A function to find the time dimension and units of a NetCDF-CF source.
#'
#' This function takes an open ncdf4 data source.
#' It returns the time units and time dimension.
#'
#' @param ncdf4_handle ncdf4 object.
#' @return A named list with time_units and the time_dim
#' @export
#' @examples
#' \dontrun{
#' Soon!
#' }
#' 
get_time_dim<-function(ncdf4_handle)
{
  if(!require("ncdf4")){
    print("trying to install ncdf4")
    install.packages("ncdf4")
    if(require(ncdf4)){
      print("ncdf4 installed and loaded")
    } else {
      stop("could not install ncdf4")
    }
  }
  if (!is.null(ncdf4_handle$dim$time$units)) {
    time_units<-strsplit(ncdf4_handle$dim$time$units, " ")[[1]]
    time_dim<-ncdf4_handle$dim$time$vals
    cal_units<-ncdf4_handle$dim$time$units
  } else if (!is.null(ncdf4_handle$dim$day$units)) {
    time_units<-strsplit(ncdf4_handle$dim$day$units, " ")[[1]]
    time_dim<-ncdf4_handle$dim$day$vals
    cal_units<-ncdf4_handle$dim$day$units
  } else stop(paste("No time dimension found. Time dimensions called time and day are supported."))
  # A hack to deal with Daymet specifying 
  if(time_dim[1]-round(time_dim[1])==0.5) time_dim<-time_dim-0.5
  return(list(time_dim=time_dim, time_units=time_units, cal_units=cal_units))
}