\name{daily_indices}
\alias{daily_indices}

\title{ Daily Climate Indices }

\description{
	\code{daily_indices} implements a suite of basic climate indices based on daily records It is written to run one year of data at a time for scalability to large gridded domains.
}

\usage{ daily_indices(tmin=NULL, tmax=NULL, prec=NULL, tmean=NULL, thresholds, time_PCICt) }

\arguments{
	\item{tmin}{a data.frame or matrix with greater than 359 and less than 367 columns representing daily minimum temperature data; rows represent different locations.}
	\item{tmax}{a data.frame or matrix as with \code{tmin} representing maximum temperature data.}
	\item{prec}{a data.frame or matrix as with \code{tmin} representing precipitation data.}
	\item{tmean}{a data.frame or matrix as with \code{tmin} representing mean temperature data.}
  \item{thresholds}{A named list with the following structure:\cr
  thresholds=list(days_tmax_abv_thresh=c(),\cr
                  days_tmin_blw_thresh=c(),\cr
                  days_prcp_abv_thresh=c(),\cr
                  longest_run_tmax_abv_thresh=c(),\cr
                  longest_run_prcp_blw_thresh=c(),\cr
                  growing_degree_day_thresh=c(),\cr
                  heating_degree_day_thresh=c(),\cr
                  cooling_degree_day_thresh=c(),\cr
                  growing_season_lngth_thresh=c())\cr
  where thresholds are in celsius or mm for temperature or precipitation respectively. Multiple thresholds can be specified as a vector and entire statistics can be omitted.}
	\item{time_PCICt}{ The time vector cooresponding to the data.frames or matrices in PCICt format.}
}

\value{ a matrix with columns representing variables requested and the number of rows(and order of them) the same as the input \code{tmin}, \code{tmax}, \code{prec} or \code{tmean}. }

\author{ David Blodgett \email{dblodgett@usgs.gov} }

\examples{

data(daily_indices_example, package='climates')
output<-daily_indices(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, tmean=tave_data, thresholds,time_PCICt)

}

