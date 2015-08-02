#' NetCDF-CF Date Intersection
#' 
#' This function takes units and values to describe a NetCDF time coordinate
#' variable and a start/end year. It returns.
#' 
#' This function is only meant to work for full calendar years.
#' 
#' @param time_units A time units string from a netCDF time coordinate
#' variable.
#' @param time_dim The values of a netCDF time coordinate variable.
#' @param start A four digit year to get the starting index for.
#' @param end A four digit year to get the ending index for.
#' @return A named list containing the following variables 
#' \enumerate{
#'  \item{'t_ind1'}{Index position for the first value in the start year.}
#'  \item{'t_ind2'}{Index position for the last value of the end year.  }
#'  \item{'time'}{Values of the time coordinate variable for the requested period.}
#'  \item{'origin'}{The calendar origin for use as the 'origin' input to chron. }
#'  }
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @export
#' @examples
#' 
#' 
#' Using ncdf4 and remote data:
#' 
#' library("ncdf4")
#' start<-1950
#' end<-1951
#' OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/prism"
#' ncdf4_handle <- nc_open(OPeNDAP_URI)
#' time_units<-strsplit(ncdf4_handle$dim$time$units, " ")[[1]]
#' time_dim<-ncdf4_handle$dim$time$vals
#' CF_date_range(time_units, time_dim, start, end)
#' 
#' Using sample data:
#' 
#' data(CF_date_range_example, package='climates')
#' CF_date_range(time_units, time_dim, start, end)
#' 
#' 
CF_date_range<-function(time_units, time_dim, start, end)
{  
  time_step<-time_units[1]
  date_origin<-time_units[3]
  time_origin<-"00:00:00"
  if(length(time_units)==4) time_origin<-time_units[4]
  cal_origin <- paste(date_origin, time_origin)
  # Convert to posixlt
  year_origin<-as.numeric(strsplit(date_origin,'-')[[1]][1])
  month_origin<-as.numeric(strsplit(date_origin,'-')[[1]][2])
  day_origin<-as.numeric(strsplit(date_origin,'-')[[1]][3])
  chron_origin<-c(month=month_origin, day=day_origin, year=year_origin)
  t_1 <- julian(strptime(paste(start,'-01-01 12:00',sep=''), '%Y-%m-%d %H:%M'), origin<-strptime(cal_origin, '%Y-%m-%d %H:%M:%S'))
  t_2 <- julian(strptime(paste(end, '-01-01 00:00', sep=''), '%Y-%m-%d %H:%M'), origin<-strptime(cal_origin, '%Y-%m-%d %H:%M:%S'))
  # Some simple time and bbox validation.
  if (t_1<head(time_dim,1)) stop(paste("Submitted start date,",start, "is before the dataset's start date,",chron(floor(head(time_dim,1)),out.format=c(dates="year-m-day"), origin=chron_origin)))
  if (t_2>tail(time_dim,1)) stop(paste("Submitted end date,",end, "is after the dataset's end date,",chron(floor(tail(time_dim,1)),out.format=c(dates="year-m-day"), origin=chron_origin)))
  if (t_1>t_2) stop('Start date must be before end date.')
  t_ind1 <- min(which(abs(time_dim-t_1)==min(abs(time_dim-t_1))))
  t_ind2 <- max(which(abs(time_dim-t_2)==min(abs(time_dim-t_2))))
  time<-time_dim[t_ind1:(t_ind2-1)]
  time_posix=chron(time,origin=chron_origin,out.format="month/day/year")
  time_posix=strptime(time_posix,"%B/%d/%Y")
  return(list(t_ind1=t_ind1, t_ind2=t_ind2, time=time, origin=chron_origin, time_posix=time_posix))
}
