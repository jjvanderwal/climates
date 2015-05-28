#convert the anomoly work to a dataframe with xyz where the z columns are the different time values
#input is a object from append.nc or extract.monthly.averages or retrieve.nc


#' Convert Objects of Class NC to a Data.frame
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param nc %% ~~Describe \code{nc} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (nc) {
#' 	#ensure nc is from retrieve.nc or similar
#' 	if (all(c('dat','tim','lat', 'lon') %in% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
#' 	#do the work
#' 	out = expand.grid(lon = nc$lon,lat = nc$lat)
#' 	for (tim in nc$tim) out[[paste('tim.',tim,sep='')]] = as.vector(nc$dat[tim,,])
#' 	return(out)
#'   }
#' 
nc2dataframe <- 
function (nc) {
	#ensure nc is from retrieve.nc or similar
	if (all(c('dat','tim','lat', 'lon') %in% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	#do the work
	out = expand.grid(lat = nc$lat,lon = nc$lon); #get all possible combination s of lat & lon
	out = matrix(c(out$lat,out$lon,nc$dat),nrow=nrow(out),ncol=length(nc$tim)+2) #append all the data
	colnames(out) = c('lat','lon',paste('tim.',nc$tim,sep='')) #set the column names
	return(out)
}


