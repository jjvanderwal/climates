\name{anom.prec}
\alias{anom.prec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
anom.prec(nc1, nc2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc1}{
%%     ~~Describe \code{nc1} here~~
}
  \item{nc2}{
%%     ~~Describe \code{nc2} here~~
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
function(nc1,nc2) { #precipitation of future (nc2) as a proportion of current (nc1)
	#ensure nc1 & nc2 are from retrieve.nc
	if (all(c('dat','tim','lat', 'lon') \%in\% names(nc1))==FALSE) stop('nc1 must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	if (all(c('dat','tim','lat', 'lon') \%in\% names(nc2))==FALSE) stop('nc2 must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	#do the work
	out = nc1; out$dat = nc2$dat / nc1$dat; return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
