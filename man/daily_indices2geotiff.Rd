\name{daily_indices2geotiff}
\alias{daily_indices2geotiff}

\title{ Daily Indices to GeoTiff }

\description{
	This function wraps the daily_indices function. It writes data to a geotiff file in the working directory.
}

\usage{ daily_indices2geotiff(tmax_data,tmin_data,prcp_data,tave_data,thresholds, coords_master, prj, year) }

\arguments{
	\item{tmin}{a data.frame or matrix with 12 or 52 columns representing monthly or weekly minimum temperature data; rows represent different locations.}
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
  \item{coordinates}{Coordinates of grid cells as output by CF_bbox_grid.}
  \item{projection}{A Proj4 string for the spatial reference of the input string.}
  \item{year}{A year to use in labeling the output file.}
}

\value{ A character vector of the files that were created.}

\author{ David Blodgett \email{dblodgett@usgs.gov} }

\examples{

data(daily_indices2geotiff_example, package='climates')
fileNames<-daily_indices2geotiff(tmax_data, tmin_data, prcp_data, tave_data, thresholds, coords_master, prj, year,time_PCICt)

}

