#' Calculate Quarter with Extremes for Temperature and Precipitation
#' 
#' \code{extremes} calculates quarter (3 month or 13 week) in wich highest and
#' lowest temperature and highest and lowest precipitation occurred given input
#' of temperature max and min and precipitation.  Calculated the same was as in
#' \code{\link{bioclim.Rd}}.
#' 
#' If "month" is specified for \code{period}, 12 columns of data are expected,
#' if "week" is specified, 52 columns are expected.
#' 
#' @param tmin a data.frame or matrix with 12 or 52 columns representing
#' monthly or weekly minimum temperature data; rows represent different
#' locations. (required)
#' @param tmax a data.frame or matrix as with \code{tmin} representing maximum
#' temperature data. (required)
#' @param prec a data.frame or matrix as with \code{tmin} representing
#' precipitation data. (required)
#' @param tmean a data.frame or matrix as with \code{tmin} representing mean
#' temperature data. (optional; will be calculated as (tmax+tmin)/2)
#' @param period can be either "month" or "week" representing the temporal
#' period for which values are calculated; see details for further description.
#' @param tiebreak determines how calls to max.col will decide ties (multiple
#' periods with same maximum). Options are "random", "first", "last" but there
#' is no min.col and which.min only finds first min.
#' @return a matrix with columns representing the number of the first month (or
#' week) of the "Warmest","Coldest","Wettest","Driest" quarter.  The number of
#' rows (and order of them) is the same as the input \code{tmin}, \code{tmax},
#' \code{prec} or \code{tmean}.
#' @author Patrick D. Lorch \email{plorch@@kent.edu}
#' @seealso \code{\link{bioclim.Rd}}
#' @keywords extremes
#' @export
#' @examples
#' \dontrun{
#' # Need to fill in to match bioclim example
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function(tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL, period = "month",tiebreak="first")
#' {
#' # 	This function finds the warmest, wettest, coldest, driest periods for each location
#' 	
#' 	tsize = NULL
#' 	m.per.indx=function(x){c(x,(x:(x+1))%%12+1)} # index for 3 month quarter
#' 	w.per.indx=function(x){c(x,(x:(x+11))%%52+1)} # index for 13 week quarter
#' 	
#' 	# Function to check for various input errors, stop program and print an error message
#' 	error.check=function(datum,datum.name,dsize=tsize){
#' 		if (is.null(datum)) 
#' 			stop(paste(datum.name,"is needed for the variables selected"))
#' 		else if (is.data.frame(datum) | is.matrix(tmin)) {
#' 			if (!(dim(datum)[2] %in% c(12, 52))) # check for correct number of columns
#' 				stop(paste(datum.name,"must have 12 or 52 columns -- one for each month or week"))
#' 			dsize = c(dsize, dim(datum)[1])
#' 		}
#' 		else 
#' 			stop(paste(datum.name,"must be a data.frame or matrix"))
#' 		return(dsize)
#' 	}
#' 	
#' 	# Check for valid input data; tmin, tmax, prec are always needed
#' 	tsize=error.check(tmin,"tmin")
#' 	tsize=error.check(tmax,"tmax")
#' 	tsize=error.check(prec,"prec")
#' 
#' 	if (is.null(tmean)) {
#' 		tmean=(tmax+tmin)/2
#' 		print("Calculated tmean as (tmax+tmin)/2")
#' 	}
#' 	else 
#' 		tsize=error.check(tmean,"tmean")
#' 	
#' 	if (!all(tsize == mean(tsize))) # Check all input vars are the same length
#' 		stop("all input data must be of the same length") # redundant?
#' 	
#' 	tsize=mean(tsize)
#' 	out = matrix(NA, nr = tsize, nc = 4)  # Set up output matrix
#' 	
#' 	if (period == "month") { # Warmest/coldest and wettest/driest quarter sums and means
#' 		tt1 = matrix(NA, nr = tsize, nc = 12)
#' 		tt2 = matrix(NA, nr = tsize, nc = 12)
#' 		for (ii in 1:12) {  # Find temperature means for 3 month quarters
#' 			tt1[, ii] = rowMeans(tmean[, m.per.indx(ii)], na.rm = T)
#' 			tt2[, ii] = rowSums(prec[, m.per.indx(ii)], na.rm = T)
#' 		}
#' 	}
#' 	else {
#' 		tt1 = matrix(NA, nr = tsize, nc = 52)
#' 		tt2 = matrix(NA, nr = tsize, nc = 52)
#' 		for (ii in 1:52) {  # Find temperature means for 13 week quarters
#' 			tt1[, ii] = rowMeans(tmean[, w.per.indx(ii)], na.rm = T)
#' 			tt2[, ii] = rowSums(prec[, w.per.indx(ii)], na.rm = T)
#' 		}
#' 	}
#' 	# 1 Warmest quarter
#' 	out[, 1] = max.col(tt1,tiebreak)
#' 	# 2 Coldest quarter; there is no min.col!; this can only find first match
#' 	out[, 2] = unname(apply(tt1, 1, which.min))
#' 	# 3 Wettest quarter
#' 	out[, 3] = max.col(tt2,tiebreak)
#' 	# 4 Driest quarter; this can only find first match
#' 	out[, 4] = unname(apply(tt2, 1, which.min))
#' 		
#' #	out=data.frame(out)
#' 	colnames(out) = c("Warmest","Coldest","Wettest","Driest")
#' 	return(out)
#'   }
#' }
extremes=function(tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL, period = "month",tiebreak="first")
{
# 	This function finds the warmest, wettest, coldest, driest periods for each location
	
	tsize = NULL
	m.per.indx=function(x){c(x,(x:(x+1))%%12+1)} # index for 3 month quarter
	w.per.indx=function(x){c(x,(x:(x+11))%%52+1)} # index for 13 week quarter
	
	# Function to check for various input errors, stop program and print an error message
	error.check=function(datum,datum.name,dsize=tsize){
		if (is.null(datum)) 
			stop(paste(datum.name,"is needed for the variables selected"))
		else if (is.data.frame(datum) | is.matrix(tmin)) {
			if (!(dim(datum)[2] %in% c(12, 52))) # check for correct number of columns
				stop(paste(datum.name,"must have 12 or 52 columns -- one for each month or week"))
			dsize = c(dsize, dim(datum)[1])
		}
		else 
			stop(paste(datum.name,"must be a data.frame or matrix"))
		return(dsize)
	}
	
	# Check for valid input data; tmin, tmax, prec are always needed
	tsize=error.check(tmin,"tmin")
	tsize=error.check(tmax,"tmax")
	tsize=error.check(prec,"prec")

	if (is.null(tmean)) {
		tmean=(tmax+tmin)/2
		print("Calculated tmean as (tmax+tmin)/2")
	}
	else 
		tsize=error.check(tmean,"tmean")
	
	if (!all(tsize == mean(tsize))) # Check all input vars are the same length
		stop("all input data must be of the same length") # redundant?
	
	tsize=mean(tsize)
	out = matrix(NA, nrow = tsize, ncol = 4)  # Set up output matrix
	
	if (period == "month") { # Warmest/coldest and wettest/driest quarter sums and means
		tt1 = matrix(NA, nrow = tsize, ncol = 12)
		tt2 = matrix(NA, nrow = tsize, ncol = 12)
		for (ii in 1:12) {  # Find temperature means for 3 month quarters
			tt1[, ii] = rowMeans(tmean[, m.per.indx(ii)], na.rm = T)
			tt2[, ii] = rowSums(prec[, m.per.indx(ii)], na.rm = T)
		}
	}
	else {
		tt1 = matrix(NA, nrow = tsize, ncol = 52)
		tt2 = matrix(NA, nrow = tsize, ncol = 52)
		for (ii in 1:52) {  # Find temperature means for 13 week quarters
			tt1[, ii] = rowMeans(tmean[, w.per.indx(ii)], na.rm = T)
			tt2[, ii] = rowSums(prec[, w.per.indx(ii)], na.rm = T)
		}
	}
	# 1 Warmest quarter
	out[, 1] = max.col(tt1,tiebreak)
	# 2 Coldest quarter; there is no min.col!; this can only find first match
	out[, 2] = unname(apply(tt1, 1, which.min))
	# 3 Wettest quarter
	out[, 3] = max.col(tt2,tiebreak)
	# 4 Driest quarter; this can only find first match
	out[, 4] = unname(apply(tt2, 1, which.min))
		
	out=data.frame(out)
	colnames(out) = c("Warmest","Coldest","Wettest","Driest")
	return(out)
}
