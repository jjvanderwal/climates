#define a function to append data of two ncdf files from retrieve.nc or at least have the tim,yy,mm,dd,
#we must assume that units, extents, variable, etc. are all the same


#' Append Temporally Disjunct NCDF data
#' 
#'  ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#'  ~~ If necessary, more details than the description above ~~
#' 
#' @param nc1  ~~Describe \code{nc1} here~~
#' @param nc2  ~~Describe \code{nc2} here~~
#' @return ...
#' @note  ~~further notes~~
#' @author  ~~who you are~~
#' @seealso  ~~objects to See Also as \code{\link{help}}, ~~~
#' @references  ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function(nc1,nc2) {
#' 	#ensure nc1 & nc2 are from retrieve.nc
#' 	if (all(c('dat','tim','yy','mm','dd', 'lat', 'lon') %in% names(nc1))==FALSE) stop('nc1 must have objects named dat, lat, lon, tim, yy, mm and dd as from retrieve.nc of clim.pact package')
#' 	if (all(c('dat','tim','yy','mm','dd', 'lat', 'lon') %in% names(nc2))==FALSE) stop('nc2 must have objects named dat, lat, lon, tim, yy, mm and dd as from retrieve.nc of clim.pact package')
#' 	#do the work
#' 	out = nc1 #copy nc1
#' 	out$dat = array(NA,dim=c(dim(nc1$dat)[1]+dim(nc2$dat)[1],dim(nc1$dat)[2],dim(nc1$dat)[3]))#redim dat
#' 	out$dat[1:dim(nc1$dat)[1],,] = nc1$dat #add dat from nc1
#' 	out$dat[(dim(nc1$dat)[1]+1):(dim(nc1$dat)[1]+dim(nc2$dat)[1]),,] = nc2$dat #add dat from nc2
#' 	#append the time information
#' 	out$tim = c(nc1$tim,nc2$tim)
#' 	out$yy = c(nc1$yy,nc2$yy)
#' 	out$mm = c(nc1$mm,nc2$mm)
#' 	out$dd = c(nc1$dd,nc2$dd)
#' 	if ('id.t' %in% names(nc1)) out$id.t = c(nc1$id.t,nc2$id.t)
#' 	if ('filename' %in% names(nc1)) out$filename = NA
#' 	if ('attributes' %in% names(nc1)) out$attributes$filename = NA
#' 	#return the data
#' 	return(out)
#'   }
#' 
append.nc = function(nc1,nc2) {
	#ensure nc1 & nc2 are from retrieve.nc
	if (all(c('dat','tim','yy','mm','dd', 'lat', 'lon') %in% names(nc1))==FALSE) stop('nc1 must have objects named dat, lat, lon, tim, yy, mm and dd as from retrieve.nc of clim.pact package')
	if (all(c('dat','tim','yy','mm','dd', 'lat', 'lon') %in% names(nc2))==FALSE) stop('nc2 must have objects named dat, lat, lon, tim, yy, mm and dd as from retrieve.nc of clim.pact package')
	#do the work
	out = nc1 #copy nc1
	out$dat = array(NA,dim=c(dim(nc1$dat)[1]+dim(nc2$dat)[1],dim(nc1$dat)[2],dim(nc1$dat)[3]))#redim dat
	out$dat[1:dim(nc1$dat)[1],,] = nc1$dat #add dat from nc1
	out$dat[(dim(nc1$dat)[1]+1):(dim(nc1$dat)[1]+dim(nc2$dat)[1]),,] = nc2$dat #add dat from nc2
	#append the time information
	out$tim = c(nc1$tim,nc2$tim)
	out$yy = c(nc1$yy,nc2$yy)
	out$mm = c(nc1$mm,nc2$mm)
	out$dd = c(nc1$dd,nc2$dd)
	if ('id.t' %in% names(nc1)) out$id.t = c(nc1$id.t,nc2$id.t)
	if ('filename' %in% names(nc1)) out$filename = NA
	if ('attributes' %in% names(nc1)) out$attributes$filename = NA
	#return the data
	class(out) = unique(c(class(out),'nc'))
	return(out)
}
