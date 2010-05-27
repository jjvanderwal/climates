\name{extend.global.data}
\alias{extend.global.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extend.global.data(nc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc}{
%%     ~~Describe \code{nc} here~~
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
function(nc) {
	#ensure nc1 & nc2 are from retrieve.nc
	if (all(c('dat','tim','lat', 'lon') \%in\% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	#do the work
	#assuming this is a global model, we need to buffer the edges to interpolate
	#adjust the latitude
	nc$lat = c(nc$lat[1]-mean(diff(nc$lat),na.rm=T),nc$lat,nc$lat[length(nc$lat)]+mean(diff(nc$lat),na.rm=T))
	if(min(nc$lat,na.rm=T)>-90) nc$lat[nc$lat==min(nc$lat,na.rm=T)] = -90
	if(max(nc$lat,na.rm=T)<90) nc$lat[nc$lat==max(nc$lat,na.rm=T)] = 90
	#adjust the longitude
	nc$lon = c(nc$lon[1]-mean(diff(nc$lon),na.rm=T),nc$lon,nc$lon[length(nc$lon)]+mean(diff(nc$lon),na.rm=T))
	if(min(nc$lon,na.rm=T)>-180) nc$lon[nc$lon==min(nc$lon,na.rm=T)] = -180
	if(max(nc$lon,na.rm=T)<180) nc$lon[nc$lon==max(nc$lon,na.rm=T)] = 180
	#populate a new set of outa with new dim of lat & lon
	out = array(0,dim=c(length(nc$tim),length(nc$lat),length(nc$lon)))
	out[1:dim(nc$dat)[1],1:dim(nc$dat)[2]+1,1:dim(nc$dat)[3]+1] = nc$dat[,,]
	#adjust the latitude data
	tmid = ceiling(dim(nc$dat)[3] / 2)
	out[,1,] = cbind(NA,nc$dat[,1,tmid:dim(nc$dat)[3]],nc$dat[,1,1:(tmid-1)],NA)
	out[,dim(out)[2],] = cbind(NA,nc$dat[,dim(nc$dat)[2],tmid:dim(nc$dat)[3]],nc$dat[,dim(nc$dat)[2],1:(tmid-1)],NA)
	#adjust the longitude information... appending ends from other side of globe
	out[,,1] = out[,,dim(out)[3]-1]
	out[,,dim(out)[3]] = out[,,2]
	#return the extended outa
	return(list(dat=out,lon=nc$lon,lat=nc$lat,tim=nc$tim))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
