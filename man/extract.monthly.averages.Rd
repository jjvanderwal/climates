\name{extract.monthly.averages}
\alias{extract.monthly.averages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract Monthly Longterm Climate Averages }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.monthly.averages(nc, min.year, max.year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc}{
%%     ~~Describe \code{nc} here~~
}
  \item{min.year}{
%%     ~~Describe \code{min.year} here~~
}
  \item{max.year}{
%%     ~~Describe \code{max.year} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(nc,min.year,max.year) {
	#ensure nc1 & nc2 are from retrieve.nc
	if (all(c('dat','tim','yy','mm', 'lat', 'lon') \%in\% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon, tim, yy and mm as from retrieve.nc of clim.pact package')
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
