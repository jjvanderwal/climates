#' @title Extract Monthly Longterm Climate Averages
#' 
#' @description need a function to create monthy averages given a set of years
#' 
#' @details give it the nc file from retrieve.nc, min year and max year
#' 
#' @param nc  ~~Describe \code{nc} here~~
#' @param min.year  ~~Describe \code{min.year} here~~
#' @param max.year  ~~Describe \code{max.year} here~~
#' @return ...
#' @note  ~~further notes~~
#' @author  ~~who you are~~
#' @seealso  ~~objects to See Also as \code{\link{help}}, ~~~
#' @references  ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @export
#' @examples
#' \dontrun{
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function(nc,min.year,max.year) {
#' 	#ensure nc1 & nc2 are from retrieve.nc
#' 	if (all(c('dat','tim','yy','mm', 'lat', 'lon') %in% names(nc))==FALSE) 
#'   stop('nc must have objects named dat, lat, lon, tim, yy and mm as from 
#'   retrieve.nc of clim.pact package')
#' 	#do the work
#' 	#track lat, long and time
#' 	lat = nc$lat
#' 	lon = nc$lon
#' 	mm = 1:12
#' 	#create an output array
#' 	out.data = array(0,dim=c(length(mm),length(lat),length(lon)))
#' 	#get the array subset which is from the start year to end year
#' 	years = which(nc$yy %in% min.year:max.year)
#' 	#cycle through all the data in those years
#' 	for (ii in years) out.data[nc$mm[ii],,] = out.data[nc$mm[ii],,] + nc$dat[ii,,]
#' 	#now get the average by deviding by the number of months
#' 	for (ii in mm) out.data[ii,,] = out.data[ii,,] / (max.year-min.year+1)
#' 	#return the information
#' 	return(list(dat=out.data,lon=lon,lat=lat,tim=mm))
#'   }
#' }
extract.monthly.averages <- function(nc,min.year,max.year) {
	#ensure nc1 & nc2 are from retrieve.nc
	if (all(c('dat','tim','yy','mm', 'lat', 'lon') %in% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon, tim, yy and mm as from retrieve.nc of clim.pact package')
	#do the work
	#track lat, long and time
	lat = nc$lat
	lon = nc$lon
	mm = 1:12; n = rep(0,12)
	#create an output array
	out = array(0,dim=c(length(mm),length(lat),length(lon)))
	#get the array subset which is from the start year to end year
	years = NULL; 
	for (ii in min.year:max.year) { for (jj in mm) { years = c(years, which(nc$yy == ii & nc$mm == jj)[1]) } }
	#cycle through all the data in those years
	for (ii in years) { out[nc$mm[ii],,] = out[nc$mm[ii],,] + nc$dat[ii,,]; n[nc$mm[ii]] = n[nc$mm[ii]] + 1 }
	#now get the average by deviding by the number of months
	for (ii in mm) out[ii,,] = out[ii,,] / n[ii]
	#return the information
	out = list(dat=out,lon=nc$lon,lat=nc$lat,tim=mm)
	class(out) = unique(c(class(out),'nc'))
	return(out)
}
