% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/extract.monthly.averages.R
\name{extract.monthly.averages}
\alias{extract.monthly.averages}
\title{Extract Monthly Longterm Climate Averages}
\usage{
extract.monthly.averages(nc, min.year, max.year)
}
\arguments{
\item{nc}{~~Describe \code{nc} here~~}

\item{min.year}{~~Describe \code{min.year} here~~}

\item{max.year}{~~Describe \code{max.year} here~~}
}
\value{
...
}
\description{
need a function to create monthy averages given a set of years
}
\details{
give it the nc file from retrieve.nc, min year and max year
}
\note{
~~further notes~~
}
\examples{
\dontrun{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(nc,min.year,max.year) {
	#ensure nc1 & nc2 are from retrieve.nc
	if (all(c('dat','tim','yy','mm', 'lat', 'lon') \%in\% names(nc))==FALSE)
  stop('nc must have objects named dat, lat, lon, tim, yy and mm as from
  retrieve.nc of clim.pact package')
	#do the work
	#track lat, long and time
	lat = nc$lat
	lon = nc$lon
	mm = 1:12
	#create an output array
	out.data = array(0,dim=c(length(mm),length(lat),length(lon)))
	#get the array subset which is from the start year to end year
	years = which(nc$yy \%in\% min.year:max.year)
	#cycle through all the data in those years
	for (ii in years) out.data[nc$mm[ii],,] = out.data[nc$mm[ii],,] + nc$dat[ii,,]
	#now get the average by deviding by the number of months
	for (ii in mm) out.data[ii,,] = out.data[ii,,] / (max.year-min.year+1)
	#return the information
	return(list(dat=out.data,lon=lon,lat=lat,tim=mm))
  }
}
}
\author{
~~who you are~~
}
\references{
~put references to the literature/web site here ~
}
\seealso{
~~objects to See Also as \code{\link{help}}, ~~~
}
\keyword{~kwd1}
\keyword{~kwd2}

