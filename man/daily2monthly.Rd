\name{daily2monthly}
\alias{daily2monthly}

\title{ Convenience utility to convert daily to monthly data. }

\description{
	Given data, an accompanying NetCDF coordinate variable, and the origin (units) for the coordinate variable, this function will return monthly resolution data.
}

\usage{ daily2monthly(daily_data, time, origin, cells=NULL) }

\arguments{
	\item{daily_data}{A matrix of daily data values.}
  \item{time}{The time indices that coorespond to the matrix of daily data.}
  \item{origin}{The origin of the time coordinate variable.}
  \item{cells}{The number of cells in the daily_data matrix. Optional.}
}

\details{
	This is a convenience function to help use daily data with theBioClim utility.
}
\value{ The data converted to monthly resolution. }

\author{ David Blodgett \email{dblodgett@usgs.gov} }

\examples{

data(daily2monthly_example, package='climates')
tmax_data<-daily2monthly(tmax_data, time_indices, origin, cells)

}

