\name{read.nc}
\alias{read.nc}
\title{ Read From a netCDF File }
\description{
	\code{read.nc} reads a netCDF file from e.g., PCMDI and picks out vectors that look like longitude, latitude and time. It returns a list containing the data and associated attributes from the netCDF file.\cr\cr
	The code here represents some, albeit slight, modification of \code{retrieve.nc} of the clim.pact package. As functions of this package require consistent structuring of outputs, we decided to present a modified copy of that function here. Indeed, much of this helpfile comes from \code{retrieve.nc}.
}

\usage{
retrieve.nc(filename=file.path("data","air.mon.mean.nc"),v.nam="AUTO",
            l.scale=FALSE,greenwich=TRUE,silent=FALSE,
            x.nam="lon",y.nam="lat",z.nam="lev",t.nam="tim",
            x.rng=NULL,y.rng=NULL,z.rng=NULL,t.rng=NULL,
            force.chron=TRUE,force365.25=FALSE,regular=TRUE,
            daysayear=365.25,forceBC=TRUE,
            use.cdfcont=FALSE,torg=NULL,t.unit=NULL,miss2na=TRUE)
}

\arguments{
    \item{filename}{name of netCDF file.}
    \item{v.nam}{name of variable. "AUTO" -> smart search.}
    \item{l.scale}{'TRUE' uses scaling.factor and add.offset.}
    \item{greenwich}{'TRUE' centres maps on Greenwhich meridian (0 deg E).}
    \item{x.nam}{Name of x-dimension.}
    \item{y.nam}{Name of y-dimension.}
    \item{z.nam}{Name of z-dimension.}
    \item{t.nam}{Name of time-axis.}
    \item{x.rng}{Region to extract.}
    \item{y.rng}{Region to extract.}
    \item{z.rng}{Region to extract.}
    \item{t.rng}{Time interval to extract.Numerical values are used to identify indeces, e.g. \code{as.numeric(1)} refers to first field, \code{as.numeric(2)} the second field, etc. Character strings, on the other hand, refers to date. E.g. "1-Jan-1990", or "1990" (see \code{\link{datestr2num}} for various formats).}
    \item{force.chron}{Check for monotonic chronological order (no
      jumping back and forth in time).}
    \item{force365.25}{TRUE forces a natural 365.25 day year as opposed to a 360-day model year. '-1' forces a 360-day year (commonly used in climate modelling).}
    \item{regular}{TRUE for regular spacing in time (.i.e. no skipping, but one field every month or one field every day).}
    \item{daysayear}{Number of days in the year on average.}
    \item{forceBC}{TRUE for not accepting year 0 (see e.g. Press et al. (1989), Numerical Recepies in Pascal, Cambridge).}
    \item{use.cdfcont}{Flag for Linux versions only: if TRUE use old lines calling 'cdfcont()'}
    \item{torg}{Time origin, such as the 'time\_origin' attribute in netCDF
      files. e.g. '15-Dec-1949'. A NULL value (default) will try to detect from
      the file header.}
    \item{t.unit}{Time unit, similar to the 'time\_unit' attribute in
      netCDF files. e.g. 'day'. A NULL value (default) will try to detect from
      the file header.}
}
\value{A list of class "nc" and "field.object" with the following data:
    \tabular{ll}{
      dat \tab a 3-D matrix with the data. \cr
      lon \tab a vector of longitudes.  \cr
      lat \tab a vector of latitudes.  \cr
      tim \tab a vector of times from time.0 (see attributes). \cr
      lev \tab a vector of vertical levels (NULL for single level). \cr
      v.name \tab variable name.\cr
      id.x \tab ID labels for the spatial grid (for mixed fields, see \code{\link{mixFields}}). \cr
      id.t \tab ID labels for the time axis (for combined fields). \cr
      yy \tab a vector of years corresponding to \code{tim}. \cr
      mm \tab a vector of months corresponding to \code{tim}. \cr
      dd \tab a vector of days corresponding to \code{tim}. \cr
      n.fld \tab number of fields (for mixed fields, see
      \code{\link{mixFields}}). \cr
      id.lon \tab ID labels along the longitudes (for mixed fields, see
      \code{\link{mixFields}}). \cr
      id.lat \tab ID labels along the latitudes (for mixed fields, see
      \code{\link{mixFields}}). \cr
      }.
}
\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }
\references{ Rasmus E. Benestad (2010). clim.pact: Climate analysis and empirical-statistical downscaling (ESD) package for monthly and daily data.. R package version 2.2-41. \url{http://CRAN.R-project.org/package=clim.pact} }
\examples{ #need to fill in }
